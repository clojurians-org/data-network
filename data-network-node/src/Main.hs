{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels,  DataKinds, TypeOperators #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified DataNetwork.Core.Types as DN
import Prelude
import Control.Lens

import Labels ((:=)(..))
import qualified Labels as L
import Labels.JSON ()

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

import qualified UnliftIO as U
import Data.String.Conversions (cs)

import Control.Concurrent (ThreadId, threadDelay, forkIO, killThread)
import System.Cron as Cron
import qualified Data.HashMap.Lazy as M

data App = App
makeLenses ''App

type Trigger = ( "in" := Chan.TBMChan B.ByteString
               , "out" := Chan.TBMChan B.ByteString )

type AppST = M.HashMap DN.FaasKey (ThreadId, DN.FaasInfo)

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
  tids <- liftIO $ Cron.execSchedule $ Cron.addJob (activeSQLScanner outChan key) cronExpr
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

activeSQLScanner :: (MonadIO m) => CC.TBMChan B.ByteString -> DN.FaasKey -> m ()
activeSQLScanner outChan key@(DN.FaasKey _ name) = do
  liftIO $ putStrLn ("activeSQLScanner:" <> cs name)
  ts <- liftIO $ getPOSIXTime
  U.atomically $ Chan.writeTBMChan outChan $ J.encode $
    DN.FaasNotifyPush ( key, "ScannerItemsEvent"
                      , J.toJSON . DN.ScannerItemsEvent $
                          [ ( #offset := "1", #task_name := "LARLUO", #task_event := "START", #ts := ts )
                          , ( #offset := "3", #task_name := "LARLUO2", #task_event := "START", #ts := ts ) ])
    
  U.atomically $ Chan.writeTBMChan outChan $ J.encode $
    DN.FaasNotifyPush ( key, "ScannerScheduleEvent", J.toJSON . DN.ScannerScheduleEvent $ ( #offset := "3", #ts := ts ))
    
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
