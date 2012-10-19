{-# LANGUAGE TemplateHaskell #-}
module SNet.Network
  ( SNet
  , Info (..)
  , location
  , createTasks
  , Location (..)
  , initChildNet
  , spawnSNet
  ) where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Default
import SNet.Stream

newtype Location = Loc Int
data Info = Info {
  _location :: Location,
  _createTasks :: Bool
}

makeLenses ''Info

instance Default Info where
  def = Info
    { _location = Loc 0
    , _createTasks = True
    }

type SNet = Stream -> StateT Info IO Stream

initChildNet :: SNet -> StateT Info IO Info
initChildNet net = do
    info <- get
    createTasks .= False
    net undefined
    createTasks .= True
    return info

spawnSNet :: MonadIO m => SNet -> Info -> Stream -> m Stream
spawnSNet net info output = liftIO $ evalStateT (net output) info
