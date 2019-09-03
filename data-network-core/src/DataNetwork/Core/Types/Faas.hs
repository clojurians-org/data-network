{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels, DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module DataNetwork.Core.Types.Faas where

import qualified DataNetwork.Core.Types.Common as DC
import Prelude
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Control.Lens

import Data.Time.Clock.POSIX (POSIXTime)

import Data.Default (Default(def))
import qualified Data.Text as T
import Labels ((:=)(..))
import qualified Labels as L
import Labels.JSON ()

import qualified Data.Aeson as J

import Data.Vinyl ((:::), (=:), Rec ((:&)))
import qualified Data.Vinyl as V


type SQLScannerLabel =
  ( "name" := T.Text
  , "desc" := T.Text
  , "cron" := DC.CronExpr
  , "sql_connect" := DC.SQLConnect
  , "sql" := T.Text
  , "increment_field" := T.Text
  , "xid" := Maybe Int64 )
  
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

instance DC.HasLabel SQLScanner where
  type instance Label SQLScanner = SQLScannerLabel
  label (SQLScanner r) = r
  

type ScannerItem = V.FieldRec '["row" ::: J.Value, "ts" ::: POSIXTime]
type ScannerSchedule = V.FieldRec '[ "offset" ::: Maybe J.Value, "ts" ::: POSIXTime ]
data SQLScannerNotifyEvent = ScannerItemsEvent [ScannerItem]
                           | ScannerScheduleEnterEvent ScannerSchedule
                           | ScannerScheduleLeaveEvent ScannerSchedule
  deriving (Generic, Show)
instance J.ToJSON SQLScannerNotifyEvent
instance J.FromJSON SQLScannerNotifyEvent

parseJScannerItems :: J.Value -> Maybe [ScannerItem]
parseJScannerItems json =
  case J.fromJSON json of
    J.Success (ScannerItemsEvent r) -> Just r
    _ -> Nothing

