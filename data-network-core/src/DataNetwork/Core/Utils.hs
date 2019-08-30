{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module DataNetwork.Core.Utils where

import Prelude
import GHC.Generics (Generic, Rep)
import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Aeson as J


import qualified Control.Monad.Trans.Resource as R
import Data.String.Conversions (cs)
import Control.Lens
import Data.Aeson.Lens as J

import qualified Data.Vinyl as V
import Data.Vinyl ((:::))
import Data.Vinyl.Functor ((:.))
import qualified Data.Vinyl.Class.Method as V

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
fieldsToJSON = undefined

recToListF :: (Applicative f, V.RFoldMap rs) => V.Rec (f :. Const a) rs -> f [a]
recToListF = undefined

instance (V.RFoldMap rs, V.RecMapMethod1 J.ToJSON f rs) => J.ToJSON (V.Rec f rs) where
  toJSON = maybe (error "") J.object . recToListF .    fieldsToJSON
-- recToJSON :: (Generic (V.Rec f rs), J.GToJSON J.Zero (Rep (V.Rec f rs))) => V.Rec f rs -> J.Value
-- recToJSON = J.Object . view (deep _Object) . J.genericToJSON J.defaultOptions

type FaasInfo = V.Rec V.ElField '["id" ::: String, "status" ::: String, "run" ::: J.Value]

data A = A FaasInfo deriving (Show, Generic)
--instance J.FromJSON A
instance J.ToJSON A
