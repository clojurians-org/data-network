{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module DataNetwork.Core.Types.RPC where

import DataNetwork.Core.Utils
import qualified DataNetwork.Core.Types.Common as DN
import Prelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Aeson as J
import Labels ((:=)(..))
import Labels.JSON ()

import Data.Hashable (Hashable(..))
import qualified Data.Vinyl as V
import Data.Vinyl ((=:), (:::))

data FaasStatus = FaasActived | FaasKilled deriving (Generic, Show)
instance J.ToJSON FaasStatus
instance J.FromJSON FaasStatus

type FaasInfo = V.Rec V.ElField '["id" :::  FaasKey, "status" ::: FaasStatus, "run" ::: J.Value]
-- , "status" := FaasStatus, "run" := J.Value )
-- type FaasInfo = "id" := FaasKey

data RPCRequest = FaasActiveReq (FaasKey, DN.CronExpr)
                | FaasKillReq FaasKey
                | FaasReadReq FaasKey
  deriving (Generic, Show)
instance J.ToJSON RPCRequest
instance J.FromJSON RPCRequest

data FaasKey = FaasKey {
    faasKeyType :: T.Text
  , faasKeyName :: T.Text
  } deriving (Generic, Show, Eq)
instance J.ToJSON FaasKey
instance J.FromJSON FaasKey
instance J.ToJSONKey FaasKey
instance J.FromJSONKey FaasKey

instance Hashable FaasKey

type EventTag = T.Text
data RPCResponse = FaasActiveRes (Either String (FaasKey, DN.CronExpr))
                 | FaasKillRes (Either String FaasKey)
                 | FaasReadRes (Either String (Maybe FaasInfo))
                 | FaasNotifyPush (FaasKey, EventTag, J.Value)
                 | FaasUnhandle RPCRequest
                 | FaasDebug T.Text
  deriving (Generic, Show)
instance J.ToJSON RPCResponse
instance J.FromJSON RPCResponse
