{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ExtendedDefaultRules, TypeApplications  #-}
{-# LANGUAGE OverloadedLabels, PolyKinds, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}

module DataNetwork.Core.Types.Common where

import Prelude
import GHC.Generics (Generic, Rep)

import qualified Data.Text as T
import qualified Data.Aeson as J
import Data.String.Conversions (cs)

import Labels ((:=)(..))
import Labels.JSON ()
import Data.Default (Default(def))

import qualified Data.HashMap.Lazy as M
import qualified Data.HashMap.Strict as MS

import Data.Aeson ((.:))
import Data.Aeson.Lens as J

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Data.Vinyl as V
import Data.Vinyl ((:::), (=:), Rec((:&)))
import Data.Vinyl.Functor ((:.))
import qualified Data.Vinyl.Class.Method as V
import qualified Data.Vinyl.Functor as V

instance Default Bool where def = True
instance Default T.Text where def = T.empty
instance Default (M.HashMap k v) where def = M.empty

instance J.ToJSON a => J.ToJSON (V.ElField '(s, a)) where
  toJSON x = J.object [(cs (V.getLabel x), J.toJSON (V.getField x))]

fieldsToJSON :: (V.RecMapMethod1 J.ToJSON f rs) => V.Rec f rs -> V.Rec (Maybe :. V.Const (T.Text, J.Value)) rs
fieldsToJSON = V.rmapMethod1 @J.ToJSON (V.Compose . aux)
  where aux x = case J.toJSON x of
                  J.Object (MS.toList -> [field]) -> Just (V.Const field)
                  _ -> Nothing

recToListF :: (Applicative f, V.RFoldMap rs) => V.Rec (f :. V.Const a) rs -> f [a]
recToListF = fmap (V.rfoldMap (pure . V.getConst)) . V.rtraverse V.getCompose

instance (V.RFoldMap rs, V.RecMapMethod1 J.ToJSON f rs) => J.ToJSON (V.Rec f rs) where
  toJSON = maybe (error "") J.object . recToListF . fieldsToJSON

instance J.FromJSON (V.FieldRec '[]) where
  parseJSON _ = return V.RNil

instance (KnownSymbol s, J.FromJSON a, J.FromJSON (V.FieldRec rs))
  => J.FromJSON (V.FieldRec (s ::: a ': rs)) where
  parseJSON o@(J.Object v) = do
    f :: a <- v .: cs (symbolVal (Proxy :: Proxy s))
    rest <- J.parseJSON o
    pure $ ((V.Label:: V.Label s) =: f) :& rest

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
  
 
utilsRepl :: IO ()
utilsRepl = do
  let a = J.toJSON (#a =: "ccc" :& #b =: (#c =: "nest" :& V.RNil) :& V.RNil)
      b = J.fromJSON a :: J.Result (V.FieldRec '["a" ::: T.Text, "b" ::: (V.FieldRec '["c" ::: T.Text])])
  print a
  print b
