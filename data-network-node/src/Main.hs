{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Lens hiding (lens)
import Labels

import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Aeson as J
import Control.Monad (forever)
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

import Control.Concurrent (threadDelay, forkIO)
import System.Cron as Cron
import qualified Data.HashMap.Lazy as M

data App = App
makeLenses ''App

type FaasHooker = ( "status" := FaasStatus
                  , "cron" := T.Text
                  , "out" := Chan.TBMChan B.ByteString)
type Trigger = ( "in" := Chan.TBMChan B.ByteString
               , "out" := Chan.TBMChan B.ByteString)
data FaasAction =  FaasActive | FaasKill
  deriving (Generic, Show)
instance J.ToJSON FaasAction
instance J.FromJSON FaasAction
data FaasStatus = Actived | Killed
  deriving (Generic, Show)
instance J.ToJSON FaasStatus
instance J.FromJSON FaasStatus

data WSRequestMessage = WSAdminReq FaasAction (T.Text, T.Text)
  deriving (Generic, Show)
instance J.ToJSON WSRequestMessage
instance J.FromJSON WSRequestMessage
data WSResponseMessage = WSAdminRes FaasAction (Either String (T.Text, T.Text))
                       | WSUnhandle WSRequestMessage
  deriving (Generic, Show)
instance J.ToJSON WSResponseMessage
instance J.FromJSON WSResponseMessage

type AppST = ( "faas" := T.Text, "status" := FaasStatus )
main :: IO ()
main = R.runResourceT $ do
  hook <- serveFaaSAsync
  liftIO $ do
    serveSnaplet defaultConfig (app hook)
    putStrLn "finished!"

mkChan :: (R.MonadResource m) =>Int -> m (R.ReleaseKey, CC.TBMChan a)
mkChan n = R.allocate (Chan.newTBMChanIO n) (U.atomically . Chan.closeTBMChan)

serveFaaSAsync :: (U.MonadUnliftIO m, R.MonadResource m)
  => m Trigger
serveFaaSAsync = do
  (inReg, inChan) <- mkChan 1000
  (outReg, outChan) <- mkChan 1000
  U.async $ do
    C.runConduit
       $ CC.sourceTBMChan inChan
      .| C.iterM (liftIO . print)
      .| (CL.mapMaybe J.decode .| C.mapM (fmap J.encode . faasHandle))
      .| CC.sinkTBMChan outChan
      .| C.sinkNull
    R.release outReg
    R.release inReg
  return (#in := inChan, #out := outChan)

faasHandle :: (R.MonadResource m) => WSRequestMessage -> m WSResponseMessage
faasHandle (WSAdminReq FaasActive faas@(cronExpr, name)) = do
  liftIO $ putStrLn "WSAdminReq handle..."
  liftIO $ Cron.execSchedule $ Cron.addJob activeSQLScanner cronExpr
  return . WSAdminRes FaasActive . Right $ faas
  
faasHandle req = return . WSUnhandle $ req

activeSQLScanner = do
  putStrLn "hello world"
  
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
     .| CC.sinkTBMChan (trigger ^. lens #in)
      )

    (C.runConduit
      $ CC.sourceTBMChan (trigger ^. lens #out)
--     .| C.iterM (liftIO . putStrLn . ("serveWS-receive:" <>) . cs)
     .| C.mapM_ (liftIO . WS.sendTextData conn)
     .| C.sinkNull
      )
  liftIO $ putStrLn "end websocket connection..."

  return ()
  
-- ["FaasActive",["0/1 * * * *","larluo"]]
