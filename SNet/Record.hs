{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module SNet.Record
  ( RecEntry (..)
  , RecType (..)
  , Record (..)
  , recTags
  , recBTags
  , recFields
  , emptyRecord
  , isTerminate
  , insert
  , lookup
  , (!)
  , merge
  , delete
  , match
  , flowInherit
  ) where

import Prelude hiding (lookup)
import Data.IntMap hiding (insert, lookup, (!), delete)
import qualified Data.IntMap as IntMap

import Control.Lens

import SNet.Pattern
import SNet.Types

recTags :: Simple Lens (Record Data) (IntMap CInt)
recTags fun (Rec t b f) = fmap (\t' -> Rec t' b f) (fun t)

recBTags :: Simple Lens (Record Data) (IntMap CInt)
recBTags fun (Rec t b f) = fmap (\b' -> Rec t b' f) (fun b)

recFields :: Simple Lens (Record Data) (IntMap Ref)
recFields fun (Rec t b f) = fmap (\f' -> Rec t b f') (fun f)

emptyRecord :: Record Data
emptyRecord = Rec empty empty empty

isTerminate :: Record a -> Bool
isTerminate Terminate = True
isTerminate _ = False

insert :: RecEntry a -> a -> Record Data -> Record Data
insert (Tag i)   val = recTags   %~ IntMap.insert i val
insert (BTag i)  val = recBTags  %~ IntMap.insert i val
insert (Field i) val = recFields %~ IntMap.insert i val

lookup :: RecEntry a -> Record Data -> Maybe a
lookup (Tag i)   = recTags   `views` IntMap.lookup i
lookup (BTag i)  = recBTags  `views` IntMap.lookup i
lookup (Field i) = recFields `views` IntMap.lookup i

(!) :: Record Data -> RecEntry a -> a
rec ! Tag i   = rec ^. recTags   ^% (IntMap.! i)
rec ! BTag i  = rec ^. recBTags  ^% (IntMap.! i)
rec ! Field i = rec ^. recFields ^% (IntMap.! i)

delete :: RecEntry a -> Record Data -> Record Data
delete (Tag i)   = recTags   %~ IntMap.delete i
delete (BTag i)  = recBTags  %~ IntMap.delete i
delete (Field i) = recFields %~ IntMap.delete i

merge :: Record Data -> Record Data -> Record Data
merge (Rec t1 b1 f1) (Rec t2 b2 f2) =
    Rec (union t1 t2) (union b1 b2) (union f1 f2)

match :: Pattern p -> Record Data -> Maybe Int
match pat rec = foldPattern pat counter (Just 0)
  where counter :: RecEntry e -> Maybe Int -> Maybe Int
        counter e = (lookup e rec >>) . fmap (+1)

flowInherit :: Pattern p -> Record Data -> Record Data -> Record Data
flowInherit pat base new = merge new $ clean pat base
  where clean :: Pattern p -> Record Data -> Record Data
        clean p = foldPattern p delete
