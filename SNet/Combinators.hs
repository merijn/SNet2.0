module SNet.Combinators
  ( (-->)
  , (!!)
  , parallel
  , (*)
  ) where

import Prelude hiding (lookup, (!!), (*), mapM)

import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Trans.State

import Data.Default
import Data.Traversable
import qualified Data.Map as Map

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
    openStream output
    task Map.empty
         (\rec -> do
            let i = rec ! tag
            stream <- gets (Map.lookup i) `onNothing` spawnChild i
            writeStream stream rec)
         (do gets Map.elems >>= mapM_ closeStream
             closeStream output)
  where spawnChild i = do (stream, _) <- spawnSNet net def output
                          modify $ Map.insert i stream
                          return stream

parallel :: VariantMap ps SNet -> SNet
parallel branchList output = do
    outputs <- mapM spawnNet branchList
    task_ (\rec -> writeStream (getBestMatch outputs rec) rec)
          (void $ mapM closeStream outputs)
  where spawnNet net = fst <$> spawnSNet net def output

(*) :: SNet -> Pattern p -> SNet
(*) net pattern output = do
    openStream output
    task Nothing
        (\rec -> case match pattern rec of
            Just _ -> writeStream output rec
            Nothing -> do new <- get `onNothing` spawnChild
                          writeStream new rec)
        (do get >>= withJust closeStream
            closeStream output)
  where childNet = net --> (net * pattern)
        spawnChild = do (stream, _) <- spawnSNet childNet def output
                        put (Just stream)
                        return stream

onNothing :: Monad m => m (Maybe a) -> m a -> m a
onNothing a b = a >>= maybe b return

withJust :: Monad m => (a -> m ()) -> Maybe a -> m ()
withJust = maybe (return ())
