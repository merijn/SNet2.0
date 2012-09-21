{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
module SNet.Stream
  ( Stream
  , newStream
  , openStream
  , readStream
  , writeStream
  , closeStream
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans

import SNet.Types

newStream :: MonadIO m => m Stream
newStream = liftIO $ liftM2 (,) (newMVar 0) newEmptyMVar

openStream :: MonadIO m => Stream -> m ()
openStream (i, _) = liftIO . modifyMVar_ i $ return . (+1)

readStream :: MonadIO m => Stream -> (forall r . Record r -> m a) -> m a
readStream (_, stream) f = do
  rec <- liftIO $ takeMVar stream
  case rec of
      WrapData r -> f r
      WrapControl r -> f r

writeStream :: MonadIO m => Stream -> Record r -> m ()
writeStream (_, stream) = liftIO . putMVar stream . wrap
  where wrap :: Record r -> WrappedRecord
        wrap (Rec t b f) = WrapData    (Rec t b f)
        wrap (Sync s)    = WrapControl (Sync s)
        wrap Trigger     = WrapControl Trigger
        wrap Terminate   = WrapControl Terminate

closeStream :: MonadIO m => Stream -> m ()
closeStream (i, stream) = liftIO . modifyMVar_ i $ \x -> do
   when (x == 1) $ putMVar stream (WrapControl Terminate)
   return (x - 1)
