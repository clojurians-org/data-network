{-# LANGUAGE NoImplicitPrelude #-}

module DataNetwork.Node.Class where

import Prelude
import Control.Monad.Trans (MonadIO)

class Scanner s where
  scan :: (MonadIO m) => s -> m ()