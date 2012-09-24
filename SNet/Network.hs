module SNet.Network
  ( SNet
  , Info (..)
  , Location (..)
  , spawnSNet
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Default
import SNet.Stream

newtype Location = Loc Int
data Info = Info { location :: Location }

instance Default Info where
  def = Info { location = Loc 0 }

type SNet = Stream -> StateT Info IO Stream

spawnSNet :: MonadIO m => SNet -> Info -> Stream -> m (Stream, Info)
spawnSNet net info output = liftIO $ runStateT (net output) info
