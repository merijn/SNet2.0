{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module SNet.Task
  ( Task
  , task
  , task_
  , box
  , syncro
  ) where

import Control.Concurrent (forkIO)
import Control.Monad.Trans
import Control.Monad.Trans.State (evalStateT, StateT)

import SNet.Record
import SNet.Pattern
import SNet.Variants
import SNet.Stream
import SNet.Interfaces
import SNet.Network

type Task a = StateT a IO ()

task_ :: MonadIO m => (Record Data -> Task ()) -> Task () -> m Stream
task_ = task ()

task :: MonadIO m => a -> (Record Data -> Task a) -> Task a -> m Stream
task state taskFun stop = do
    input <- newStream
    liftIO . forkIO $ evalStateT (loop input) state
    return input
  where loop stream =
          readStream stream $ \case
               rec@Rec {..} -> taskFun rec >> loop stream
               Sync s       -> loop s
               Trigger      -> loop stream
               Terminate    -> stop

type family BoxFun (l :: [*])
type instance BoxFun '[] = IO ()
type instance BoxFun (h ': t) = h -> BoxFun t

apply :: Pattern p -> BoxFun p -> Record Data -> IO ()
apply Nil        boxfun _   = boxfun
apply (Cons h t) boxfun rec = apply t (boxfun (rec ! h)) rec

box :: Handle hnd
    => Pattern p
    -> Variants ps n
    -> (hnd -> BoxFun p)
    -> SNet
box pattern variants boxfun output = do
    openStream output
    hnd <- newHandle variants writeOut
    task_ (apply pattern . boxfun `withHandle` hnd)
          (destroyHandle hnd >> closeStream output)
    where writeOut :: Record Data -> Record Data -> IO ()
          writeOut old new = writeStream output $ flowInherit pattern old new

syncro :: Pattern p -> Stream -> SNet
syncro = undefined
