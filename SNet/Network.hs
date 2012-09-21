module SNet.Network
  ( SNet
  , Info (..)
  , Location (..)
  ) where

import Control.Monad.Trans.State
import Data.Default
import SNet.Stream

newtype Location = Loc Int
data Info = Info { location :: Location }

instance Default Info where
  def = Info { location = Loc 0 }

type SNet = Stream -> StateT Info IO Stream
