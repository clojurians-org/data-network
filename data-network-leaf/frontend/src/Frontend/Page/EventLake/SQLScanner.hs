{-# LANGUAGE NoImplicitPrelude, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Frontend.Page.EventLake.SQLScanner (eventLake_sqlScanner) where

import Common.WebSocketMessage
import Common.Types
import Common.ExampleData

import Frontend.FrontendStateT
import Frontend.Widget
import Frontend.Class

import Prelude
import Reflex.Dom.Core

import Control.Monad.Fix (MonadFix)

eventLake_sqlScanner
  :: forall t m .
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , HasFrontendState t (FaaSCenter, WSResponseMessage) m
     , EventWriter t [WSRequestMessage] m)
  => m ()
eventLake_sqlScanner = do
  divClass "ui segment basic" $ do
    pageHeader "SQL扫描器" ["增量字段扫描", "cron调度"]
  
  return ()
