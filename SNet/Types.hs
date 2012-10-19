{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module SNet.Types
  ( Stream
  , Ref
  , CInt
  , RecEntry (..)
  , RecType (..)
  , Record (..)
  , WrappedRecord (..)
  ) where

import Control.Concurrent
import Data.IntMap
import Foreign.C.Types

import Text.Read (lift, readPrec)
import qualified Text.ParserCombinators.ReadP as ReadP

type Stream = (MVar Int, MVar WrappedRecord)
type Ref = () --Either (ForeignPtr ()) (Location, WordPtr)

data RecEntry a where
  Tag :: Int -> RecEntry CInt
  BTag :: Int -> RecEntry CInt
  Field :: Int -> RecEntry Ref

data RecType = Control | Data

data Record (t :: RecType) where
  Rec :: IntMap CInt -> IntMap CInt -> IntMap Ref -> Record Data
  Sync :: Stream -> Record Control
  Trigger :: Record Control
  Terminate :: Record Control

data WrappedRecord where
  WrapControl :: Record Control -> WrappedRecord
  WrapData :: Record Data -> WrappedRecord

instance Eq (Record Data) where
    Rec t b f == Rec t' b' f' = t == t' && b == b' && f == f'

instance Ord (Record Data) where
    compare (Rec t b f) (Rec t' b' f')
      | t < t' = LT
      | t > t' = GT
      | b < b' = LT
      | b > b' = GT
      | f < f' = LT
      | f > f' = GT
      | otherwise = EQ

instance Show (Record Data) where
    show (Rec t b _) = "Rec {(" ++ show t ++ ") (" ++ show b ++ ")}"

instance Read (Record Data) where
    readPrec = do
        string "Rec {("
        tags <- readPrec
        string ") ("
        btags <- readPrec
        string ")}"
        return $ Rec tags btags empty
      where string = lift . ReadP.string
