{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module SNet.Interfaces
  ( Handle (..)
  , C.C'Handle
  , C.Ptr
  ) where

import Control.Monad.Trans

import SNet.Record
import SNet.Variants

import qualified SNet.Interfaces.C as C

infixl 1 `withHandle`

class Handle hnd where
  newHandle :: MonadIO m
            => Variants ps n
            -> (Record Data -> Record Data -> IO ())
            -> m hnd

  withHandle :: MonadIO m
             => (hnd -> Record Data -> IO ())
             -> hnd
             -> Record Data
             -> m ()

  destroyHandle :: MonadIO m => hnd -> m ()

instance Handle (C.Ptr C.C'Handle) where
  newHandle = C.newHandle
  withHandle = C.withHandle
  destroyHandle = C.destroyHandle
