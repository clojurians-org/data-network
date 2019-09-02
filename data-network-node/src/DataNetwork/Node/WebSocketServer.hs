{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, TypeApplications  #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedLabels, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module DataNetwork.Node.WebSocketServer where

import qualified DataNetwork.Core as DC
import Prelude
import Control.Lens

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified TextShow as T
import Data.String.Conversions (cs)
import Text.Heredoc (str)
import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as M

import Data.Time.Clock.POSIX (getPOSIXTime)

import Control.Monad (forever)
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import qualified UnliftIO as U
import qualified Control.Concurrent.MVar.Lifted as L
import qualified Control.Concurrent.Async.Lifted as L
import qualified Control.Monad.Trans.Resource as R

import qualified Control.Concurrent.STM.TBMChan as STM

import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.TMChan as CC

import qualified Network.WebSockets as WS
import qualified System.Cron as Cron

import Data.Vinyl ((:::), (=:), Rec ((:&)))
import qualified Data.Vinyl as V
import Control.Concurrent (ThreadId, killThread)

instance MonadBase b m => MonadBase b (R.ResourceT m) where
    liftBase = lift . liftBase

instance MonadBaseControl b m => MonadBaseControl b (R.ResourceT m) where
  type StM (R.ResourceT m) a = StM m a
  liftBaseWith f = R.withInternalState $ \reader' ->
    liftBaseWith $ \runInBase ->
      f $ runInBase . (\t -> R.runInternalState t reader')    
  restoreM base = R.withInternalState $ const $ restoreM base
  
type WebSocketState = M.HashMap DC.FaasKey (V.FieldRec '[ "tid" ::: ThreadId
                                                         , "id" ::: DC.FaasKey
                                                         , "status" ::: DC.FaasStatus
                                                         , "run" ::: M.HashMap T.Text J.Value ])
defWebSocketState = M.empty

mkChan :: (R.MonadResource m) => Int -> m (R.ReleaseKey, CC.TBMChan a)
mkChan n = R.allocate (STM.newTBMChanIO n) (U.atomically . STM.closeTBMChan)

serve :: L.MVar WebSocketState -> WS.ServerApp
serve mv pending = R.runResourceT $ do
  conn <- liftIO $ WS.acceptRequest pending
  liftIO $ T.printT "websocket connection accepted.."
  
  (reg, brokerChan) <- mkChan 1000
  let leafSource = forever $ liftIO (WS.receiveData @B.ByteString conn) >>= C.yield
      leafSink = C.awaitForever $ liftIO . WS.sendTextData @B.ByteString conn
  U.concurrently_
    (C.runConduit
       $ leafSource
      .| C.iterM (liftIO . T.printT . ("node-serve-in:" <>))
      .| C.concatMap J.decode .| C.mapM (handleRequest brokerChan mv) .| C.map J.encode
      .| CC.sinkTBMChan brokerChan )
    (C.runConduit
       $ CC.sourceTBMChan brokerChan
      .| C.iterM (liftIO . T.printT . ("node-serve-out:" <>))       
      .| leafSink )
  liftIO $ T.printT "end websocket connection..."
  return ()

handleRequest :: (MonadIO m, MonadBaseControl IO m) => CC.TBMChan B.ByteString -> L.MVar WebSocketState -> DC.RPCRequest-> m DC.RPCResponse
handleRequest brokerChan stmv (DC.FaasActiveReq faas@(key, DC.CronExpr cron)) = do
  liftIO $ T.printT "FaasActiveReq  handle..."
  tid <- fmap head . liftIO $ Cron.execSchedule $ Cron.addJob (activeSQLScanner brokerChan stmv key) cron
  L.modifyMVar_ stmv (return . createEntry key tid)
  return . DC.FaasActiveRes . Right $ faas
  where
    createEntry key tid= set (at key) . Just $
      (  #tid =: tid :& #id =: key :& #status =: DC.FaasActived :& #run =: M.empty :& V.RNil )

handleRequest brokerChan stmv (DC.FaasKillReq key) = do
  st <- L.readMVar stmv
  st ^?! (ix key . V.rlensf #tid . to (liftIO . killThread))
  L.modifyMVar_ stmv $ return . set (ix key . V.rlensf #status) DC.FaasKilled
  return . DC.FaasKillRes . Right $ key

handleRequest _ _ _ = undefined

activeSQLScanner :: (MonadIO m, MonadBase IO m, R.MonadUnliftIO m) => CC.TBMChan B.ByteString -> L.MVar WebSocketState -> DC.FaasKey -> m ()
activeSQLScanner brokerChan stmv key@(DC.FaasKey _ name) = do
  let 
    sql = [str| SELECT t2.RUN_END_DATE as OFFSET
              |      , t1.JOB_NAME
              |      , t2.EXECUTE_STATE
              |   FROM SCH_CURR_JOB t1
              |  INNER JOIN SCH_CURR_JOBSTATE t2
              |     ON t1.JOB_SEQ = T2.JOB_SEQ
              |]
--  liftIO $ 
  st <- L.readMVar stmv
  liftIO $ putStrLn ("activeSQLScanner:" <> cs name)
  tsEnter <- liftIO $ getPOSIXTime

  sinkEvent $  DC.FaasNotifyPush
    ( key, "ScannerScheduleEnterEvent"
    , J.toJSON $ DC.ScannerScheduleEnterEvent (#offset =: "3" :& #ts =: tsEnter :& V.RNil) )

  (rs, mv') <- DC.aboveOracleOffset (Just J.Null) (DC.Credential "10.129.35.227" 1521 "SCHNEW" "SCHNEW")  "EDWDB" $ sql
  {--
  U.atomically $ Chan.writeTBMChan outChan $ J.encode $
    DN.FaasNotifyPush ( key, "ScannerItemsEvent"
                      , J.toJSON . DN.ScannerItemsEvent $
                          [ ( #offset := "1", #task_name := "LARLUO", #task_event := "START", #ts := ts )
                          , ( #offset := "3", #task_name := "LARLUO2", #task_event := "START", #ts := ts ) ])
  --}

  tsLeave <- liftIO $ getPOSIXTime    
  sinkEvent $ DC.FaasNotifyPush
    ( key, "ScannerScheduleLeaveEvent"
    , J.toJSON $ DC.ScannerScheduleLeaveEvent ( #offset =: "3" :& #ts =: tsLeave :& V.RNil) )
  where sinkEvent = U.atomically . STM.writeTBMChan brokerChan . J.encode

wsRepl :: IO ()
wsRepl = do
  let a = (#a =: "aa" :& #b =: "bbb" :& V.RNil)
      b = a ^. V.rlensf #a
  T.printT b
