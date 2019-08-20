{-# LANGUAGE NoImplicitPrelude, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels, DataKinds, TypeOperators #-}

module DataNetwork.Core.Types.Faas where

import Prelude

import qualified Data.Text as T
import Labels ((:=)(..))
import qualified Labels as L
import Labels.JSON ()

type SQLScanner = ("name" := T.Text)