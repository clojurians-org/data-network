{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels, DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module DataNetwork.Core.Types.Common where

import Prelude
import GHC.Generics (Generic)

import qualified Data.Text as T
import qualified Data.Aeson as J
import Data.String.Conversions (cs)

import Labels ((:=)(..))
import Labels.JSON ()
import Data.Default (Default(def))

import qualified Data.HashMap.Lazy as M

instance Default Bool where def = True
instance Default T.Text where def = T.empty
instance Default (M.HashMap k v) where def = M.empty

class HasLabel v where
  type family Label v
  label :: v -> Label v
  

data Credential = Credential {
    hostName :: T.Text
  , hostPort :: Int
  , username :: T.Text
  , password :: T.Text
  } deriving (Generic, Show)
instance J.ToJSON Credential
instance J.FromJSON Credential
instance Default Credential

credential :: T.Text -> T.Text -> T.Text -> Credential
credential host username password = do
  let (hostName, xs) = T.breakOn ":" host
      hostPort = if (T.null xs) then 22 else (read . cs . T.tail) xs
  Credential hostName hostPort username password

data CronExpr = CronExpr T.Text deriving (Generic, Show)
instance J.ToJSON CronExpr
instance J.FromJSON CronExpr
instance Default CronExpr where def = CronExpr "0/5 * * * *"

data SQLType = PostgreSQL | Oracle | MySQL
  deriving (Generic, Show)
instance J.ToJSON SQLType
instance J.FromJSON SQLType

data DatabaseName = DatabaseName T.Text
  deriving (Generic, Show)
instance J.ToJSON DatabaseName
instance J.FromJSON DatabaseName
instance Default DatabaseName

type SQLConnectLabel =
  ( "type" := SQLType
  , "credential" := Credential
  , "database_name" := DatabaseName)
  
data SQLConnect = SQLConnect SQLConnectLabel deriving (Generic, Show)
instance J.ToJSON SQLConnect
instance J.FromJSON SQLConnect
instance Default SQLConnect where
  def = SQLConnect ( #type := PostgreSQL
                   , #credential := def
                   , #database_name := def )
        
instance HasLabel SQLConnect where
  type instance Label SQLConnect = SQLConnectLabel
  label (SQLConnect v) = v
  
 
