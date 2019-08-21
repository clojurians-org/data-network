{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels, DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module DataNetwork.Core.Types.Faas where

import qualified DataNetwork.Core.Types.Common as DN
import Prelude
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Control.Lens

import Data.Default (Default(def))
import qualified Data.Text as T
import Labels ((:=)(..))
import qualified Labels as L
import Labels.JSON ()

import qualified Data.Aeson as J


type SQLScannerLabel =
  ( "name" := T.Text
  , "desc" := T.Text
  , "cron" := DN.CronExpr
  , "sql_connect" := DN.SQLConnect
  , "sql" := T.Text
  , "increment_field" := T.Text
  , "xid" := Maybe Int64)
  
data SQLScanner = SQLScanner SQLScannerLabel
  deriving (Generic, Show)


instance J.ToJSON SQLScanner
instance J.FromJSON SQLScanner
instance Default SQLScanner where
  def = SQLScanner ( #name := ""
                   , #desc := ""
                   , #cron := def
                   , #sql_connect := def
                   , #sql := ""
                   , #increment_field := ""
                   , #xid := Nothing )

instance DN.HasLabel SQLScanner where
  type instance Label SQLScanner = SQLScannerLabel
  label (SQLScanner r) = r

