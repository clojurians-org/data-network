{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveGeneric, ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels,  DataKinds, TypeOperators #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified DataNetwork.Core as DC
import qualified DataNetwork.Node as Lib

import Prelude
import Control.Lens

import qualified Data.ByteString.Lazy as B
import qualified Data.Conduit.TMChan as CC
import qualified TextShow as T

import Text.Heredoc (str)
import qualified Data.HashMap.Lazy as M

import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))
import qualified Control.Concurrent.MVar.Lifted as L
import qualified Control.Monad.Trans.Resource as R

import qualified Snap.Core as S
import qualified Snap.Http.Server as S
import qualified Network.WebSockets.Snap as S
import qualified Snap.Snaplet as SS

import Data.Vinyl ((:::), (=:), Rec ((:&)))
import qualified Data.Vinyl as V

data App = App
makeLenses ''App

main :: IO ()
main = R.runResourceT $ do
  (reg, brokerChan) <- DC.mkChan 1000
  appStmv <- L.newMVar Lib.defWebSocketState
  liftIO $ do
    SS.serveSnaplet (S.defaultConfig & S.setPort 1111) $
      app brokerChan appStmv
  liftIO $ putStrLn "finished!"
  
app :: CC.TBMChan B.ByteString -> L.MVar Lib.WebSocketState -> SS.SnapletInit App App
app brokerChan stmv = SS.makeSnaplet "data-network-node"
                          "p2p distributed fn server"
                           Nothing $ do
  SS.addRoutes [("ws", S.runWebSocketsSnap $ Lib.serve brokerChan stmv)]
  return App

