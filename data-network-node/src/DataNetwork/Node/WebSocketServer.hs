{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, TypeApplications  #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedLabels, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, GADTs #-}

module DataNetwork.Node.WebSocketServer (serve) where

import qualified DataNetwork.Core as DC
import Prelude
import Control.Lens

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified TextShow as T
import Data.String.Conversions (cs)
import qualified Data.Aeson as J
import qualified Data.HashMap.Lazy as M

import Control.Monad (forever)
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))
import Control.Monad.Trans.Control (MonadBaseControl)
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
import Control.Concurrent (ThreadId)

instance MonadBaseControl b m => MonadBaseControl b (R.ResourceT m) where
  type StM (R.ResourceT m) a = StM m a
  
type GlobalState = M.HashMap DC.FaasKey (V.FieldRec '[ "tid" ::: ThreadId
                                                     , "id" ::: DC.FaasKey
                                                     , "status" ::: DC.FaasStatus
                                                     , "run" ::: M.HashMap T.Text J.Value ])

mkChan :: (R.MonadResource m) => Int -> m (R.ReleaseKey, CC.TBMChan a)
mkChan n = R.allocate (STM.newTBMChanIO n) (U.atomically . STM.closeTBMChan)

serve :: L.MVar GlobalState -> WS.ServerApp
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

handleRequest :: (MonadIO m, MonadBaseControl IO m) => CC.TBMChan B.ByteString -> L.MVar GlobalState -> DC.RPCRequest-> m DC.RPCResponse
handleRequest brokerChan stmv (DC.FaasActiveReq faas@(key, DC.CronExpr cron)) = do
  liftIO $ T.printT "FaasActiveReq  handle..."
  tid <- fmap head . liftIO $ Cron.execSchedule $ Cron.addJob (activeSQLScanner brokerChan key) cron
  L.modifyMVar_ stmv (return . createEntry key tid)
  undefined
  where
    createEntry key tid= set (at key) . Just $
      (  #tid =: tid :& #id =: key :& #status =: DC.FaasActived :& #run =: M.empty :& V.RNil )

handleRequest brokerChan stmv (DC.FaasKillReq key) = do
  undefined
handleRequest _ _ _ = undefined


activeSQLScanner :: (MonadIO m) => CC.TBMChan B.ByteString -> DC.FaasKey -> m ()
activeSQLScanner = undefined
