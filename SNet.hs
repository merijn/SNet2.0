{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
module SNet
  ( runSNet
  , runSNetCustom
  , SNet
  , SNetIn
  , SNetOut
  , globIn
  , globOut
  , dummyIn
  , dummyOut
  , module SNet.Interfaces
  , Variants (..)
  , VariantMap (..)
  , Pattern (..)
  , RecEntry (..)
  , Record (Rec)
  , RecType (Data)
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
import Control.Monad.State

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

type SNetIn a = MVar a -> Stream -> IO a
type SNetOut a = MonadIO m => MVar a -> m Stream

globIn :: SNetIn ()
globIn stop output = do
  openStream output
  forever $
    handle eof $ do
      rec <- readLn :: IO (Record Data)
      writeStream output rec
  where eof e = if isEOFError e
                   then do closeStream output
                           takeMVar stop
                   else ioError e

globOut :: SNetOut ()
globOut mvar = taskIO () (liftIO . print) (liftIO stop)
  where stop = putMVar mvar ()

dummyIn :: [Record Data] -> SNetIn [Record Data]
dummyIn inputList stop output = do
    openStream output
    mapM_ (writeStream output) inputList
    closeStream output
    takeMVar stop

dummyOut :: SNetOut [Record Data]
dummyOut mvar = do
    taskIO [] (modify . (:)) (get >>= stop)
  where stop = liftIO . putMVar mvar . reverse

runSNetCustom :: SNetIn a -> SNetOut a -> SNet -> IO a
runSNetCustom snetin snetout net = do
    stopMVar <- newEmptyMVar
    input <- snetout stopMVar >>= spawnSNet net def
    snetin stopMVar input

runSNet :: SNet -> IO ()
runSNet = runSNetCustom globIn globOut
