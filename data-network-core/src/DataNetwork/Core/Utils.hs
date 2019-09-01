{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveGeneric, ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module DataNetwork.Core.Utils where

import Prelude
import GHC.Generics (Generic, Rep)
import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Aeson as J
import Data.Aeson ((.:))
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)


import qualified Control.Monad.Trans.Resource as R
import Data.String.Conversions (cs)
import Control.Lens
import Data.Aeson.Lens as J

import qualified Data.HashMap.Strict as H

import qualified Data.Vinyl as V
import Data.Vinyl ((:::), (=:), Rec((:&)))
import Data.Vinyl.Functor ((:.))
import qualified Data.Vinyl.Class.Method as V
import qualified Data.Vinyl.Functor as V

tshow :: (Show a) => a -> T.Text
tshow = cs . show

bracketR :: forall a b c m. R.MonadResource m
  => IO a -> (a -> IO c) -> (a -> m b) -> m b
bracketR alloc free inside = do
  (key, seed) <- R.allocate alloc (void . free)
  res <- inside seed
  R.release key
  return res

instance J.ToJSON a => J.ToJSON (V.ElField '(s, a)) where
  toJSON x = J.object [(cs (V.getLabel x), J.toJSON (V.getField x))]

fieldsToJSON :: (V.RecMapMethod1 J.ToJSON f rs) => V.Rec f rs -> V.Rec (Maybe :. Const (T.Text, J.Value)) rs
fieldsToJSON = V.rmapMethod1 @J.ToJSON (V.Compose . aux)
  where aux x = case J.toJSON x of
                  J.Object (H.toList -> [field]) -> Just (Const field)
                  _ -> Nothing
  
recToListF :: (Applicative f, V.RFoldMap rs) => V.Rec (f :. Const a) rs -> f [a]
recToListF = fmap (V.rfoldMap (pure . getConst)) . V.rtraverse V.getCompose

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

utilsRepl :: IO ()
utilsRepl = do
  let a = J.toJSON (#a =: "ccc" :& #b =: "ddd" :& V.RNil)
      b = J.fromJSON a :: J.Result (V.FieldRec ["a" ::: T.Text, "b" ::: T.Text])
  print a
  print b
