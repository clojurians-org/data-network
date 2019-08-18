{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens hiding (lens)
import Labels

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
import Data.Conduit ((.|))
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.TMChan as CC

import qualified UnliftIO as U
import Data.String.Conversions (cs)

data App = App
makeLenses ''App

type Trigger = ( "in" := Chan.TBMChan B.ByteString
               , "out" := Chan.TBMChan B.ByteString )
main :: IO ()
main = do
  hook <- serveFaaSAsync
  serveSnaplet defaultConfig (app hook)
  putStrLn "finished!"

mkChan :: (R.MonadResource m) =>Int -> m (R.ReleaseKey, CC.TBMChan a)
mkChan n = R.allocate (Chan.newTBMChanIO n) (U.atomically . Chan.closeTBMChan)

serveFaaSAsync :: (U.MonadUnliftIO m)
  => m Trigger
serveFaaSAsync = R.runResourceT $ do
  (inReg, inChan) <- mkChan 1000
  (outReg, outChan) <- mkChan 1000
  U.async $ do
    liftIO $ putStrLn "start async serveFaaSAsync ..."
    C.runConduit
       $ CC.sourceTBMChan inChan
      .| C.iterM (liftIO . print)
      .| C.mapM faasHandle
      .| CC.sinkTBMChan outChan
      .| C.sinkNull
    liftIO $ putStrLn "end async serveFaaSAsync ..."      
    R.release inReg
    R.release outReg
  return (#in := inChan, #out := outChan)

faasHandle :: (R.MonadResource m) => B.ByteString -> m B.ByteString
faasHandle bs = do
  liftIO $ putStrLn (cs bs)
  return  ("faasHandle:" <> bs)

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
  U.async $ C.runConduit
    $ (forever $ liftIO (WS.receiveData conn) >>= C.yield)
   .| C.iterM (liftIO . print)
   .| C.map (id @B.ByteString)
   .| CC.sinkTBMChan (trigger ^. lens #in)

  C.runConduit
    $ CC.sourceTBMChan (trigger ^. lens #out)
   .| C.mapM_ (liftIO . WS.sendTextData conn)
   .| C.sinkNull
  liftIO $ putStrLn "end websocket connection..."
  return ()
  
