{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DataNetwork.Core.Utils where

import Prelude
import Control.Monad (void)
import qualified Data.Text as T
import qualified Control.Monad.Trans.Resource as R
import Data.String.Conversions (cs)

tshow :: (Show a) => a -> T.Text
tshow = cs . show

bracketR :: forall a b c m. R.MonadResource m
  => IO a -> (a -> IO c) -> (a -> m b) -> m b
bracketR alloc free inside = do
  (key, seed) <- R.allocate alloc (void . free)
  res <- inside seed
  R.release key
  return res
