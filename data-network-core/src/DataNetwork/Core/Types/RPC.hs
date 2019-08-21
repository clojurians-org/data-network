{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module DataNetwork.Core.Types.RPC where

import qualified DataNetwork.Core.Types.Common as DN
import Prelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Aeson as J
import Labels ((:=)(..))
import Labels.JSON ()


data FaasStatus = FaasActived | FaasKilled deriving (Generic, Show)
instance J.ToJSON FaasStatus
instance J.FromJSON FaasStatus

type FaasInfo = ( "name" := T.Text, "cron" := DN.CronExpr, "status" := FaasStatus )

data RPCRequest = FaasActiveReq (T.Text, DN.CronExpr)
                | FaasKillReq T.Text
                | FaasReadReq T.Text
  deriving (Generic, Show)
instance J.ToJSON RPCRequest
instance J.FromJSON RPCRequest

data RPCResponse = FaasActiveRes (Either String (T.Text, DN.CronExpr))
                 | FaasKillRes (Either String T.Text)
                 | FaasReadRes (Either String (Maybe FaasInfo))
                 | FaasNotifyRes' (Either String (T.Text, J.Value))
                 | FaasUnhandle RPCRequest
                 | FaasDebug T.Text
  deriving (Generic, Show)
instance J.ToJSON RPCResponse
instance J.FromJSON RPCResponse
