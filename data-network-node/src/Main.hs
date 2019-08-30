{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveGeneric, ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels,  DataKinds, TypeOperators #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified DataNetwork.Core as DN
import Prelude
import Control.Lens

--import GHC

import Labels ((:=)(..))
import qualified Labels as L
import Labels.JSON ()
import Text.Heredoc (str)

import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Data.Maybe (isJust, fromJust)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Aeson as J
import Control.Monad (forever, when)
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))

import qualified Control.Concurrent.STM.TBMChan as Chan
import qualified Network.WebSockets as WS
import Snap.Core (MonadSnap(..))
import Snap.Http.Server (defaultConfig, setPort)
import Snap.Snaplet (
    SnapletInit(..)
  , serveSnaplet, makeSnaplet
  , addRoutes
  )
import Network.WebSockets.Snap (runWebSocketsSnap)
import qualified Control.Monad.Trans.Resource as R
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.TMChan as CC


import Data.String.Conversions (cs)

import Control.Concurrent (ThreadId, threadDelay, forkIO, killThread)
import System.Cron as Cron
import qualified Data.HashMap.Lazy as M

import Data.Vinyl ((=:), Rec(..))
import qualified Data.Vinyl as V
import qualified Data.Vinyl.Generics as V

import qualified Control.Concurrent.MVar.Lifted as L
import Control.Monad.Trans.Control (MonadTransControl(..), MonadBaseControl(..))
import qualified UnliftIO as U


data App = App
makeLenses ''App

type Trigger = ( "in" := Chan.TBMChan B.ByteString
               , "out" := Chan.TBMChan B.ByteString )

type AppST = M.HashMap DN.FaasKey (ThreadId, DN.FaasInfo)

-- runlens :: DN.FaasKey -> Lens' AppST J.Value
-- runlens k = ix k . _2 

main :: IO ()
main = R.runResourceT $ do
  appST <- U.newMVar M.empty
  hook <- (serveFaaSAsync appST)
  liftIO $ do
    serveSnaplet (defaultConfig & setPort 1111) (app hook)
    putStrLn "finished!"

mkChan :: (R.MonadResource m) =>Int -> m (R.ReleaseKey, CC.TBMChan a)
mkChan n = R.allocate (Chan.newTBMChanIO n) (U.atomically . Chan.closeTBMChan)

serveFaaSAsync :: (U.MonadUnliftIO m, R.MonadResource m)
  => U.MVar AppST -> m Trigger
serveFaaSAsync stM = do
  (inReg, inChan) <- mkChan 1000
  (outReg, outChan) <- mkChan 1000
  U.async $ do
    C.runConduit
       $ CC.sourceTBMChan inChan
      .| C.iterM (liftIO . print)
      .| (CL.mapMaybe J.decode .| C.mapM (fmap J.encode . faasHandle stM outChan))
      .| C.iterM (liftIO . print)
      .| CC.sinkTBMChan outChan
    R.release outReg
    R.release inReg
  return (#in := inChan, #out := outChan)

faasHandle :: (R.MonadResource m, U.MonadUnliftIO m)
  => U.MVar AppST -> Chan.TBMChan B.ByteString -> DN.RPCRequest -> m DN.RPCResponse
faasHandle stM outChan (DN.FaasActiveReq faas@(key, DN.CronExpr cronExpr)) = do
  liftIO $ putStrLn "FaasActiveReq handle..."
  tids <- liftIO $ Cron.execSchedule $ Cron.addJob (activeSQLScanner outChan stM key) cronExpr
  U.modifyMVar_ stM $ return . set (at key)
      (Just (head tids, ( #id := key
                        , #status := DN.FaasActived
                        , #run := J.Null)))
  return . DN.FaasActiveRes . Right $ faas
  
faasHandle stM outChan (DN.FaasKillReq key) = do
  st <- U.readMVar stM
  let tidMaybe = st ^. (at key . to (fmap fst))
  when (isJust tidMaybe) $ liftIO . killThread . fromJust $ tidMaybe
  U.modifyMVar_ stM $ return . set (ix key . _2 . L.lens #status) DN.FaasKilled
  return . DN.FaasKillRes . Right $ key
  
faasHandle stM outChan (DN.FaasReadReq faas) = do
  U.readMVar stM <&> (^. at faas . to (DN.FaasReadRes . Right . fmap snd))
  
-- faasHandle stM outChan req = return . WSUnhandle $ req

activeSQLScanner :: (U.MonadUnliftIO m, MonadBaseControl IO m) =>
  CC.TBMChan B.ByteString -> U.MVar AppST -> DN.FaasKey -> m ()
activeSQLScanner outChan appStMv key@(DN.FaasKey _ name) = do
  let 
    sql = [str| SELECT t2.RUN_END_DATE as OFFSET
              |      , t1.JOB_NAME
              |      , t2.EXECUTE_STATE
              |   FROM SCH_CURR_JOB t1
              |  INNER JOIN SCH_CURR_JOBSTATE t2
              |     ON t1.JOB_SEQ = T2.JOB_SEQ
              |]
--  liftIO $ 
  appSt <- L.readMVar appStMv
  liftIO $ putStrLn ("activeSQLScanner:" <> cs name)
  tsEnter <- liftIO $ getPOSIXTime

  sinkEvent $  DN.FaasNotifyPush
    ( key, "ScannerScheduleEnterEvent"
    , J.toJSON . DN.ScannerScheduleEnterEvent $ ( #offset := "3", #ts := tsEnter ) )

  (rs, mv') <- DN.aboveOracleOffset (Just J.Null) (DN.Credential "10.129.35.227" 1521 "SCHNEW" "SCHNEW")  "EDWDB" $ sql
  {--
  U.atomically $ Chan.writeTBMChan outChan $ J.encode $
    DN.FaasNotifyPush ( key, "ScannerItemsEvent"
                      , J.toJSON . DN.ScannerItemsEvent $
                          [ ( #offset := "1", #task_name := "LARLUO", #task_event := "START", #ts := ts )
                          , ( #offset := "3", #task_name := "LARLUO2", #task_event := "START", #ts := ts ) ])
  --}

  tsLeave <- liftIO $ getPOSIXTime    
  sinkEvent $ DN.FaasNotifyPush
    ( key, "ScannerScheduleLeaveEvent"
    , J.toJSON . DN.ScannerScheduleLeaveEvent $ ( #offset := "3", #ts := tsLeave ) )
  where sinkEvent = U.atomically . Chan.writeTBMChan outChan . J.encode
    
--    DN.FaasDebug $ name
  
app :: Trigger -> SnapletInit App App
app trigger = makeSnaplet "data-network-node"
                          "p2p distributed fn server"
                          Nothing $ do
  addRoutes [("ws", runWebSocketsSnap $ serveWS trigger)]
  return App

serveWS :: Trigger -> WS.ServerApp
serveWS trigger pending = R.runResourceT $ do
  conn <- liftIO $ do
    putStrLn "websocket connection accepted ..."
    WS.acceptRequest pending
  U.concurrently_ 
    (C.runConduit
      $ (forever $ liftIO (WS.receiveData conn) >>= C.yield)
--     .| C.iterM (liftIO . putStrLn . ("serveWS-request:" <>) . cs)
     .| C.map (id @B.ByteString)
     .| CC.sinkTBMChan (trigger ^. L.lens #in)
      )

    (C.runConduit
      $ CC.sourceTBMChan (trigger ^. L.lens #out)
--     .| C.iterM (liftIO . putStrLn . ("serveWS-receive:" <>) . cs)
     .| C.mapM_ (liftIO . WS.sendTextData conn)
     .| C.sinkNull
      )
  liftIO $ putStrLn "end websocket connection..."

  return ()

-- {"tag":"WSInfoReq","contents":"SQLScanner"}
-- WSAdminReq FaasActive ("0/1 * * * *", "SQLScanner")
-- {"tag":"WSAdminReq","contents":["FaasActive",["0/1 * * * *","SQLScanner"]]}
-- {"tag":"WSAdminReq","contents":["FaasKill",["0/1 * * * *","SQLScanner"]]}

mainRepl :: IO ()
mainRepl = do
  let r = #a =: "aaa" :& #b =: "bbb" :& V.RNil
      j = DN.recToJSON r

  putStrLn (show j)
  putStrLn "finish"
