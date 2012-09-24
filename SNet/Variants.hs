{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
module SNet.Variants
  ( Variants (..)
  , unsafeGetPattern
  , variantsToList
  , VariantMap (..)
  , getBestMatch
  ) where

import Prelude hiding (foldr)
import Control.Applicative
import Data.Foldable
import Data.Traversable

import SNet.Pattern
import SNet.Record

data Nat = Succ Nat | Zero

data Variants :: [[*]] -> Nat -> * where
  Empty   :: Variants '[] Zero
  Variant :: Pattern p -> Variants ps n -> Variants (p ': ps) (Succ n)

unsafeGetPattern :: Integral i
                 => Variants ps n
                 -> i
                 -> (forall p . Pattern p -> r)
                 -> r
unsafeGetPattern Empty              _ _ = error "Bounds violation"
unsafeGetPattern (Variant p _)      0 f = f p
unsafeGetPattern (Variant _ s)      n f = unsafeGetPattern s (n-1) f

variantsToList :: Variants ps n -> (forall p . Pattern p -> r) -> [r]
variantsToList Empty          _ = []
variantsToList (Variant p ps) f = f p : variantsToList ps f

data VariantMap :: [[*]] -> * -> * where
  EmptyMap   :: VariantMap '[] v
  MapVariant :: Pattern p -> v -> VariantMap ps v -> VariantMap (p ': ps) v

instance Foldable (VariantMap ps) where
  foldr _ z EmptyMap = z
  foldr f z (MapVariant _ v ps) = f v (foldr f z ps)

instance Functor (VariantMap ps) where
  fmap _ EmptyMap = EmptyMap
  fmap f (MapVariant p v ps) = MapVariant p (f v) (fmap f ps)

instance Traversable (VariantMap ps) where
  traverse _ EmptyMap = pure EmptyMap
  traverse f (MapVariant p v ps) = MapVariant p <$> f v <*> traverse f ps

getBestMatch :: VariantMap ps v -> Record Data -> v
getBestMatch vmap rec = snd $ helper vmap
  where helper :: VariantMap ps v -> (Maybe Int, v)
        helper EmptyMap = (Nothing, error "No match found!")
        helper (MapVariant p v ps) = maxBy fst (match p rec, v) (helper ps)
        maxBy f x y = if f x >= f y then x else y
