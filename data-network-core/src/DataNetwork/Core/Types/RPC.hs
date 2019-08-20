{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module DataNetwork.Core.Types.RPC where

import Prelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Aeson as J
import Labels ((:=)(..))
import Labels.JSON ()

data CronExpr = CronExpr T.Text deriving (Generic, Show)
instance J.ToJSON CronExpr
instance J.FromJSON CronExpr

data FaasStatus = FaasActived | FaasKilled deriving (Generic, Show)
instance J.ToJSON FaasStatus
instance J.FromJSON FaasStatus

type FaasInfo = ( "name" := T.Text, "cron" := CronExpr, "status" := FaasStatus )

data RPCRequest = FaasActiveReq (T.Text, CronExpr)
                | FaasKillReq T.Text
                | FaasReadReq T.Text
  deriving (Generic, Show)
instance J.ToJSON RPCRequest
instance J.FromJSON RPCRequest

data RPCResponse = FaasActiveRes (Either String (T.Text, CronExpr))
                 | FaasKillRes (Either String T.Text)
                 | FaasReadRes (Either String (Maybe FaasInfo))
                 | FaasNotifyRes' (Either String (T.Text, J.Value))
                 | FaasUnhandle RPCRequest
                 | FaasDebug T.Text
  deriving (Generic, Show)
instance J.ToJSON RPCResponse
instance J.FromJSON RPCResponse
