{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module DataNetwork.Core.Types
  ( module DataNetwork.Core.Types.RPC
  , Credential (..)
  , credential
  ) where

import Prelude
import GHC.Generics (Generic)

import qualified Data.Text as T
import qualified Data.Aeson as J
import Data.String.Conversions (cs)
import DataNetwork.Core.Types.RPC

data Credential = Credential {
    hostName :: T.Text
  , hostPort :: Int
  , username :: T.Text
  , password :: T.Text
  } deriving (Generic, Show)
instance J.ToJSON Credential
instance J.FromJSON Credential

credential :: T.Text -> T.Text -> T.Text -> Credential
credential host username password = do
  let (hostName, xs) = T.breakOn ":" host
      hostPort = if (T.null xs) then 22 else (read . cs . T.tail) xs
  Credential hostName hostPort username password
