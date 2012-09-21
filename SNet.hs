{-# LANGUAGE DataKinds #-}
module SNet
  ( runSNet
  , module SNet.Interfaces
  , Variants (..)
  , Pattern (..)
  , RecEntry (..)
  , CInt (..)
  , syncro
  , box
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

import Data.Default
import Foreign.C.Types
import System.IO.Error

import SNet.Interfaces

import SNet.Task
import SNet.Pattern
import SNet.Variants
import SNet.Record
import SNet.Stream
import SNet.Network

globOut :: MonadIO m => MVar () -> m Stream
globOut stop = task_ (liftIO . print) (liftIO $ putMVar stop ())

globIn :: MVar () -> Stream -> IO ()
globIn stop output = do
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
