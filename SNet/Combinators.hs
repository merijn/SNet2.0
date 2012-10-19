{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module SNet.Combinators
  ( (-->)
  , (!!)
  , parallel
  , (*)
  ) where

import Prelude hiding (lookup, (!!), (*))

import Data.Foldable (traverse_)
import Control.Monad.State
import qualified Data.Map as Map

import Control.Lens

import Foreign.C.Types

import SNet.Record hiding (lookup, insert)
import SNet.Network
import SNet.Pattern
import SNet.Variants
import SNet.Stream
import SNet.Task

(-->) :: SNet -> SNet -> SNet
(-->) = (<=<)

(!!) :: SNet -> RecEntry CInt -> SNet
(!!) net tag output = do
    info <- initChildNet net
    task Map.empty
         (openStream output)
         (\rec -> do
            let i = rec ! tag
            stream <- zoom (at i) $ ifNothing (spawnChild info)
            writeStream stream rec)
         (do get >>= traverse_ closeStream
             closeStream output)
  where spawnChild info = spawnSNet net info output

parallel :: VariantMap ps SNet -> SNet
parallel branchList output = do
    outputs <- traverse ($ output) branchList
    task_ (\rec -> writeStream (getBestMatch outputs rec) rec)
          (traverse_ closeStream outputs)

(*) :: SNet -> Pattern p -> SNet
(*) net pattern output = do
    info <- initChildNet net
    task Nothing
        (openStream output)
        (\rec -> case match pattern rec of
            Just _ -> writeStream output rec
            Nothing -> do stream <- ifNothing (spawnChild info)
                          writeStream stream rec)
        (do get >>= maybe (return ()) closeStream
            closeStream output)
  where starNet = net --> (net * pattern)
        spawnChild info = spawnSNet starNet info output

ifNothing :: MonadState (Maybe v) m => m v -> m v
ifNothing new = do
    val <- get >>= \case
        Nothing -> new
        Just v -> return v
    put (Just val)
    return val
