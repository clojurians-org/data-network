{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}


module Frontend.Page.DataNetwork.EventPulse
  (dataNetwork_eventPulse) where

import Common.Api
import Common.WebSocketMessage
import Common.Types
import Common.ExampleData
import Frontend.FrontendStateT
import Frontend.Widget
import Frontend.Class
import Prelude

import Reflex.Dom.Core hiding ((=:))
import Control.Monad (forM_, void)
import Control.Monad.Fix (MonadFix)

import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Data.Map as M
import Data.String.Conversions (cs)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (MVar, newMVar, putMVar, modifyMVar, modifyMVar_, readMVar, threadDelay)
import Control.Lens hiding (lens)
import qualified Labels as L
import Labels (lens) 
import qualified Data.Vinyl as V
import Data.Vinyl ((:::), Rec((:&)), (=:))
import qualified Data.Aeson as J

import Control.Arrow ((>>>))

trEventPulse :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t EventPulse -> m ()
trEventPulse eventPulseD = do
  tdDynToggle (eventPulseD <&> epEnable)
  tdDynInput (eventPulseD <&> epName)
  tdDynInput (eventPulseD <&> epDesc)
  tdDynInput (constDyn "")
  tdDynInput (constDyn "")
  return ()

trDataCircuitValue :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t DataCircuitValue -> m ()
trDataCircuitValue dcivD = do
  tdDynToggle (dcivD <&> dcivEnable)
  tdDynInput (dcivD <&> snd . dcivLinkedDataCircuit)    
  tdDynInput (dcivD <&> dcivName)
  tdDynInput (dcivD <&> dcivDesc)
  return ()

dataNetwork_eventPulse
  :: forall t m .
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , HasFrontendState t (FaaSCenter, WSResponseMessage) m
     , EventWriter t [WSRequestMessage] m)
  =>  m ()
dataNetwork_eventPulse = do
  (stD, msgD) :: (Dynamic t FaaSCenter, Dynamic t WSResponseMessage) <- splitDynPure <$> askFrontendState
  let epsD = stD <&> (^.. lens #dataNetwork . lens #eventPulses . each)
      epActiveE = fforMaybe (updated msgD) $ \case
        EventPulseARES r -> Just r
        _ -> Nothing

      epRunE = fforMaybe (updated msgD) $ \case
        AsyncEventPulseActive r -> Just ( #name =: V.rvalf #name r
                                       :& #event =: "事件脉冲激活"
                                       :& #payload =: ""
                                       :& #ts =: V.rvalf #ts r
                                       :& V.RNil )
        AsyncDataCircuitBegin r ->
          Just ( #name =: (V.rvalf #event_pulse r <> " :> " <> V.rvalf #name r)
              :& #event =: "数据电路开始运行"
              :& #payload =: ""
              :& #ts =: V.rvalf #ts r
              :& V.RNil )
        AsyncDataCircuitEnd r ->
          Just ( #name =: (V.rvalf #event_pulse r <> " :> " <> V.rvalf #name r)
              :& #event =: "数据电路完成运行"
              :& #payload =: V.rvalf #result r
              :& #ts =: V.rvalf #ts r
              :& V.RNil )
        _ -> Nothing

  divClass "ui segment basic" $ do
    pageHeader "事件脉冲" ["事件源触发事件脉冲", "事件脉冲激活数据电路"]

  epE <- divClass "ui segment basic" $ do
    elClass "table" "ui selectable table" $ do
      theadList ["启用", "名称", "描述", "运行统计", "创建日期", "操作"]
      e0 <- (trE $ createIcon >> trEventPulse (constDyn (def :: EventPulse))) <&> tagPromptlyDyn (return def)
      e1 <- fmap (switchDyn . fmap leftmost) . simpleList epsD $ \v -> do
        trE <- trEB $ do
          selectE
          trEventPulse v
          divClass "ui icon buttons" $ do
            activeE <- buttonClass "ui button" $ elClass "i" "play blue icon" blank
            killE <- buttonClass "ui button" $ elClass "i" "pause blue icon" blank
            tellEventSingle $ flip tagPromptlyDyn activeE $
              EventPulseAREQ <$> (v <&> epName)
        return (tagPromptlyDyn v trE)
      return $ leftmost [e0, e1]

  divClass "ui hidden divider" blank
  
  dcivsD <- holdDyn [] (epE <&> epDataCircuitValues)
  
  dblClickedD <- holdDyn Nothing (Just () <$ epE)
  dyn (maybe blank (const (selectorWidget dcivsD)) <$> dblClickedD)  

  -- epActiveD <- holdDyn "" (epActiveE <&> cs . show)
  -- activeWidget epActiveD

  epRunsD <- foldDyn ($) [] $ mergeWith (.)
    [ epRunE <&> \x xs -> x :  xs ]
  divClass "ui segment basic" $ do
    elClass "table" "ui selectable table" $ do
    el "thead" $ el "tr" $ trHeadList ["时点", "类型", "名称", "数据"]
    simpleList epRunsD $ \epRun -> el "tr" $ do
      el "td" $ dynText (epRun <&> iso8601TimeFormat . V.rvalf #ts)
      el "td" $ dynText (epRun <&> V.rvalf #event)            
      el "td" $ dynText (epRun <&> V.rvalf #name)
      el "td" $ dynText (epRun <&> V.rvalf #payload)       
  
  return ()
  where
    activeWidget epActiveD = do
      divClass "ui segment basic" $ do
        divClass "ui brown ribbon label" $ text "运行结果"
        divClass "ui form field" $ dynEditor epActiveD
      
    selectorWidget dcivsD = do
      divClass "ui segment basic" $ divClass "ui grid" $ do
        dcivE <- divClass "eight wide column" $ navWidget dcivsD
        dcivD <- holdDyn def dcivE   
        divClass "eight wide column" $ contentWidget dcivD

      return ()
    navWidget dcivsD = do
      elClass "table" "ui selectable table collapsing" $ do
        theadList ["启用", "数据电路", "名称", "描述"]
        e0 <- (trE $ createIcon >> trDataCircuitValue (constDyn (def :: DataCircuitValue))) <&> tagPromptlyDyn (return (def::DataCircuitValue))
        e1 <- switchDyn . fmap leftmost <$> simpleList dcivsD
                (\x -> (trE $ selectE >> trDataCircuitValue x) <&> tagPromptlyDyn x)
        return $ leftmost [e0, e1]
    contentWidget dcivD = do
      divClass "ui top attached segment" $ do
        elClass "h4" "ui header" $ text "数据电路值详情"
      divClass "ui attached segment" $ divClass "ui form" $ do
        divClass "field" $ do
          el "label" $ text "筛选器"
          dynInputB (dcivD <&> (dcivGuard >>> J.encode >>> cs))
        divClass "three fields" $ do
          divClass "field" $ el "label" (text "状态容器")
            >> dynInputB (dcivD <&> (dcivLinkedDataSandbox >>> ldsaStateContainers >>> map snd >>> show >>> cs))
          divClass "field" $ el "label" (text "数据源")
            >> dynInputB (dcivD <&> (dcivLinkedDataSandbox >>> ldsaDataSources >>> map snd >>> show >>> cs))
          divClass "field" $ el "label" (text "数据服务")
            >> dynInputB (dcivD <&> (dcivLinkedDataSandbox >>> ldsaDataServices >>> map snd >>> show >>> cs))
          
      
