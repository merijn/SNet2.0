{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module SNet.Task
  ( Task
  , taskIO
  , task
  , task_
  , box
  , syncro
  ) where

import Control.Concurrent (forkIO)
import Control.Monad.Trans
import Control.Monad.Trans.Reader (ask, runReaderT, ReaderT)
import Control.Monad.Trans.State (evalStateT, StateT)

import Control.Lens

import SNet.Record
import SNet.Pattern
import SNet.Variants
import SNet.Stream
import SNet.Interfaces
import SNet.Network

type Task init state = ReaderT init (StateT state IO) ()

task' :: MonadIO m
      => state
      -> m init
      -> (Record Data -> Task init state)
      -> Task init state
      -> m Stream
task' state before taskFun after = do
    before' <- before
    input <- newStream
    liftIO . forkIO $ evalStateT (runReaderT (loop input) before') state
    return input
  where loop stream =
          readStream stream $ \case
               rec@Rec {..} -> taskFun rec >> loop stream
               Sync s       -> loop s
               Trigger      -> loop stream
               Terminate    -> after

taskIO :: MonadIO m
       => state
       -> (Record Data -> Task () state)
       -> Task () state
       -> m Stream
taskIO state = task' state (return ())

task :: state
     -> StateT Info IO init
     -> (Record Data -> Task init state)
     -> Task init state
     -> StateT Info IO Stream
task state before taskFun after = do
    run <- use createTasks
    if run
       then task' state before taskFun after
       else return undefined

task_ :: (Record Data -> Task () ())
      -> Task () ()
      -> StateT Info IO Stream
task_ = task () (return ())

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
    task ()
         (do openStream output
             newHandle variants writeOut)
         (\rec -> do hnd <- ask
                     apply pattern . boxfun `withHandle` hnd $ rec)
         (do ask >>= destroyHandle
             closeStream output)
    where writeOut :: Record Data -> Record Data -> IO ()
          writeOut old new = writeStream output $ flowInherit pattern old new

syncro :: Pattern p -> Stream -> SNet
syncro = undefined
