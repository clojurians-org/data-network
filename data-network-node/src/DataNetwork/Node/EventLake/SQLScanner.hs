{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


module DataNetwork.Node.EventLake.SQLScanner where

import DataNetwork.Types
import DataNetwork.Node.Class (Scanner(..))
import Prelude
import GHC.Generics (Generic)

import qualified Data.Text as T
import qualified Data.Aeson as J

import Labels
data SQLScanner = SQLScanner [( "event" := T.Text
                              , "credential" := Credential)]


instance Scanner SQLScanner where
  scan = undefined
