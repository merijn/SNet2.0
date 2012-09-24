{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
module SNet.Interfaces.C
  ( C'Handle
  , Ptr
  , newHandle
  , withHandle
  , destroyHandle
  ) where

import Prelude hiding (length)
import Control.Monad.Trans
import Foreign
import Foreign.C.Types

import SNet.Record
import SNet.Pattern
import SNet.Variants
import SNet.Interfaces.CHandle

type OutFun = Ptr () -> CInt -> Ptr IntPtr -> IO ()
foreign import ccall "wrapper" mkOutFun :: OutFun -> IO (FunPtr OutFun)

convert :: RecEntry a -> IntPtr -> a
convert (Tag _) = fromIntegral
convert (BTag _) = fromIntegral
convert (Field _) = error "To be implemented"

addRecEntry :: RecEntry e -> IntPtr -> Record Data -> Record Data
addRecEntry e = insert e . convert e

outputRecord :: Variants ps n
             -> (Record Data -> Record Data -> IO ())
             -> Ptr ()
             -> CInt
             -> Ptr IntPtr
             -> IO ()
outputRecord variants snetOut recPtr i intPtrValues = do
    baseRec <- deRefStablePtr . castPtrToStablePtr $ recPtr
    values  <- peekArray (withPattern length) intPtrValues
    snetOut baseRec $ withPattern (constructRec values)
  where withPattern :: (forall p. Pattern p -> a) -> a
        withPattern = unsafeGetPattern variants i
        constructRec = foldWithPattern addRecEntry emptyRecord

newHandle :: MonadIO m
          => Variants ps n
          -> (Record Data -> Record Data -> IO ())
          -> m (Ptr C'Handle)
newHandle variants snetOut = liftIO $ do
    outFun  <- mkOutFun $ outputRecord variants snetOut
    lengths <- newArray patLengths
    values  <- mallocArray $ fromIntegral (maximum patLengths)
    new $ C'Handle
      { c'Handle'lengths = lengths
      , c'Handle'values  = values
      , c'Handle'record  = nullPtr
      , c'Handle'snetOut = outFun
      }
  where patLengths = variantsToList variants length

withHandle :: MonadIO m
           => (Ptr C'Handle -> Record Data -> IO ())
           -> Ptr C'Handle
           -> Record Data
           -> m ()
withHandle boxfun hndPtr record = liftIO $ do
    stableRec <- newStablePtr record
    poke recPtr $ castStablePtrToPtr stableRec

    boxfun hndPtr record

    poke recPtr nullPtr
    freeStablePtr stableRec
  where recPtr = p'Handle'record hndPtr

destroyHandle :: MonadIO m => Ptr C'Handle -> m ()
destroyHandle hndPtr = liftIO $ do
  hnd <- peek hndPtr
  free . c'Handle'lengths $ hnd
  free . c'Handle'values  $ hnd
  freeHaskellFunPtr . c'Handle'snetOut $ hnd
  free hndPtr
