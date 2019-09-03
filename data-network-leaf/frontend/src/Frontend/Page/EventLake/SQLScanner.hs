{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels, TypeOperators, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Frontend.Page.EventLake.SQLScanner (eventLake_sqlScanner) where

import qualified DataNetwork.Core.Types as DN
import Common.WebSocketMessage
import Common.Types
import Common.ExampleData
import Common.Api

import Frontend.FrontendStateT
import Frontend.Widget
import Frontend.Class

import Prelude
import Reflex.Dom.Core

import Data.String.Conversions (cs)
import qualified Data.Aeson as J
import qualified Data.Aeson.Lens as J
import qualified TextShow as T
import qualified Labels as L
import Control.Lens


import Data.Functor ((<&>))
import Control.Monad.Fix (MonadFix)

import Data.Vinyl ((:::), (=:), Rec ((:&)))
import qualified Data.Vinyl as V

trSQLScanner :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t DN.SQLScanner -> m ()
trSQLScanner scannerD = do
  tdDynInput (scannerD <&> L.get #name . DN.label )
  tdDynInput (scannerD <&> L.get #desc . DN.label )
  tdDynInput (scannerD <&> tshow . L.get #cron . DN.label )
  tdDynInput (constDyn "")
  tdDynInput (scannerD <&> L.get #name . DN.label )  
  return ()

trSQLScannerItem :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t DN.ScannerItem -> m ()
trSQLScannerItem scannerItemD = do
  el "td" $ dynText (scannerItemD <&> (^. V.rlensf #row . J.key "OFFSET" . to tshow))
  el "td" $ dynText (scannerItemD <&> (^. V.rlensf #row . to (cs . J.encode)))  
  el "td" $ dynText (scannerItemD <&> (^. V.rlensf #ts . to iso8601TimeFormat))
  return ()
  
eventLake_sqlScanner
  :: forall t m .
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , HasFrontendState t (FaaSCenter, WSResponseMessage) m
     , EventWriter t [WSRequestMessage] m)
  => m ()
eventLake_sqlScanner = do
  (stD, msgD) :: (Dynamic t FaaSCenter, Dynamic t WSResponseMessage) <- splitDynPure <$> askFrontendState
  let scannersD = stD <&> (^.. L.lens #eventLake . L.lens #sqlScanners . each)
      scannerItemsE = fforMaybe (updated msgD) $ \case
        (NodeRPCRes (DN.FaasNotifyPush
                     (DN.FaasKey "SQLScanner" name, "ScannerItemsEvent", payload)))
          -> DN.parseJScannerItems payload
        _ -> Nothing
  divClass "ui segment basic" $ do
    pageHeader "SQL扫描器" ["增量字段扫描", "cron调度"]

  divClass "ui segment basic" $ do
    scannerE <- elClass "table" "ui selectable table" $ do
      theadList ["名称", "描述", "Cron表达式", "最近一次扫描时间", "事件脉冲", "操作"]
      e0 <- (trEB $ createIcon >> trSQLScanner (constDyn (def :: DN.SQLScanner)) )
              <&> tagPromptlyDyn (return def)
      e1 <- fmap (switchDyn . fmap leftmost) . simpleList scannersD $ \v -> do
        
        trE <- trEB $ do
          selectE >> trSQLScanner v
          text "active"          
          divClass "ui icon buttons" $ do
            activeE <- buttonClass "ui button" $ elClass "i" "play blue icon" blank
            killE <- buttonClass "ui button" $ elClass "i" "pause blue icon" blank
            tellEventSingle $ flip tagPromptlyDyn activeE $ do
              name <- v <&> DN.FaasKey "SQLScanner" . L.get #name . DN.label
              cron <- v <&> L.get #cron . DN.label
              return . NodeRPCReq . DN.FaasActiveReq $ (name, cron)
            tellEventSingle $ flip tagPromptlyDyn killE $ do
              name <- v <&> DN.FaasKey "SQLScanner" . L.get #name . DN.label
              return . NodeRPCReq . DN.FaasKillReq $ name
        return (tagPromptlyDyn v trE)   
      return $ leftmost [e0, e1]
    scannerD <- holdDyn def scannerE

    divClass "ui hidden divider" blank
    divClass "ui grid" $ do
      divClass "six wide column" $ divClass "ui form" $ do
        divClass "field" $ do
          el "label" $ text "SQL脚本"
          dynEditor (scannerD <&> L.get #sql . DN.label)
      divClass "eight wide column" $ divClass "ui form" $
        divClass "field" $ do
          el "label" $ text "运行结果"
          elClass "table" "ui selectable table" $ do
--            el "thead" $ el "tr" $ trHeadList ["偏移量", "任务名称", "事件类型"]
            blank
    let credentialD = scannerD <&> L.get #credential . DN.label . L.get #sql_connect . DN.label
    (submitD, submitE) <- loginLineB credentialD
    blank

  scannerItemsD <- foldDyn ($) [] $ mergeWith (.)
    [ scannerItemsE <&> \xs' xs -> xs' <>  xs ]
  divClass "ui segment basic" $ do
    elClass "table" "ui selectable table" $ do
    el "thead" $ el "tr" $ trHeadList ["偏移量", "扫描数据", "扫描时点"]
    simpleList scannerItemsD $ \scannerItem ->
      el "tr" (trSQLScannerItem scannerItem)
  return ()
