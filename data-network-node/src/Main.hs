{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified DataNetwork.Core.Types as Node
import Prelude
import Control.Lens

import Labels ((:=)(..))
import qualified Labels as L
import Labels.JSON ()


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
import Snap.Http.Server (defaultConfig)
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

type AppST = M.HashMap T.Text (ThreadId, Node.FaasInfo)

main :: IO ()
main = R.runResourceT $ do
  appST <- U.newMVar M.empty
  hook <- (serveFaaSAsync appST)
  liftIO $ do
    serveSnaplet defaultConfig (app hook)
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
  => U.MVar AppST -> Chan.TBMChan B.ByteString -> Node.RPCRequest -> m Node.RPCResponse
faasHandle stM outChan (Node.FaasActiveReq faas@(name, Node.CronExpr cronExpr)) = do
  liftIO $ putStrLn "FaasActiveReq handle..."
  tids <- liftIO $ Cron.execSchedule $ Cron.addJob (activeSQLScanner outChan) cronExpr
  U.modifyMVar_ stM $ return . set (at name)
      (Just (head tids, ( #name := name
                        , #cron := Node.CronExpr cronExpr
                        , #status := Node.FaasActived)))
  return . Node.FaasActiveRes . Right $ faas
  
faasHandle stM outChan (Node.FaasKillReq name) = do
  st <- U.readMVar stM
  let tidMaybe = st ^. (at name . to (fmap fst))
  when (isJust tidMaybe) $ liftIO . killThread . fromJust $ tidMaybe
  U.modifyMVar_ stM $ return . set (ix name . _2 . L.lens #status) Node.FaasKilled
  return . Node.FaasKillRes . Right $ name
  
faasHandle stM outChan (Node.FaasReadReq faas) = do
  U.readMVar stM <&> (^. at faas . to (Node.FaasReadRes . Right . fmap snd))
  
-- faasHandle stM outChan req = return . WSUnhandle $ req


activeSQLScanner outChan = do
  U.atomically $ Chan.writeTBMChan outChan (J.encode . Node.FaasDebug $ "hello world")
  
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
