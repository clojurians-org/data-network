{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
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

data App = App
makeLenses ''App

main :: IO ()
main = do
  handle <- serveFaaSAsync
  serveSnaplet defaultConfig app
  putStrLn "Hello, Haskell!"

serveFaaSAsync :: (U.MonadUnliftIO m)
  => m ( "in" := Chan.TBMChan B.ByteString
       , "out" := Chan.TBMChan B.ByteString)
serveFaaSAsync = R.runResourceT $ do
  (inReg, inChan) <- mkChan 1000
  (outReg, outChan) <- mkChan 1000
  U.async $ do
    C.runConduit
       $ CC.sourceTBMChan inChan
      .| CC.sinkTBMChan outChan
      .| C.sinkNull
    R.release inReg
    R.release outReg
  return (#in := inChan, #out := outChan)
  where
    mkChan :: (R.MonadResource m) =>Int -> m (R.ReleaseKey, CC.TBMChan a)
    mkChan n = R.allocate (Chan.newTBMChanIO n) (U.atomically . Chan.closeTBMChan)
  
app :: SnapletInit App App
app = makeSnaplet "data-network-node" "p2p distributed fn server" Nothing $ do
  addRoutes [("ws", runWebSocketsSnap serveWS)]
  return App

serveWS :: WS.ServerApp
serveWS pending = do
  putStrLn "websocket connection accepted ..."
  conn <- WS.acceptRequest pending
  C.runConduit
    $ (forever $ liftIO (WS.receiveData conn) >>= C.yield)
    .| C.map (id @B.ByteString)
    .| C.mapM_ (WS.sendTextData conn)
    .| C.sinkNull
  
