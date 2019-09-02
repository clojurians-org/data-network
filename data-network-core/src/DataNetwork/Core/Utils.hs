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

tshow :: (Show a) => a -> T.Text
tshow = cs . show

bracketR :: forall a b c m. R.MonadResource m
  => IO a -> (a -> IO c) -> (a -> m b) -> m b
bracketR alloc free inside = do
  (key, seed) <- R.allocate alloc (void . free)
  res <- inside seed
  R.release key
  return res
