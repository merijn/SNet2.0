{-# LANGUAGE DataKinds #-}
module SNet
  ( runSNet
  , module SNet.Interfaces
  , Variants (..)
  , VariantMap (..)
  , Pattern (..)
  , RecEntry (..)
  , CInt (..)
  , syncro
  , box
  , (-->)
  , (!!)
  , parallel
  , (*)
  ) where

import Prelude hiding ((!!), (*))

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

import Data.Default
import Foreign.C.Types
import System.IO.Error

import SNet.Interfaces

import SNet.Combinators
import SNet.Network
import SNet.Pattern
import SNet.Record
import SNet.Stream
import SNet.Task
import SNet.Variants

globOut :: MonadIO m => MVar () -> m Stream
globOut stop = task_ (liftIO . print) (liftIO $ putMVar stop ())

globIn :: MVar () -> Stream -> IO ()
globIn stop output =
  forever $
    handle eof $ do
      rec <- readLn :: IO (Record Data)
      writeStream output rec
  where eof e = if isEOFError e
                   then takeMVar stop
                   else ioError e

runSNet :: SNet -> IO ()
runSNet net = do
    stopMVar <- newEmptyMVar
    input <- evalStateT (globOut stopMVar >>= net) def
    globIn stopMVar input
