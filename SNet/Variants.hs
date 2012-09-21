{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
module SNet.Variants
  ( Variants (..)
  , unsafeGetPattern
  , mapVariants
  ) where

import SNet.Pattern

data Nat = Succ Nat | Zero

data Variants :: [[*]] -> Nat -> * where
  Empty :: Variants '[] Zero
  Variant :: Pattern p -> Variants ps n -> Variants (p ': ps) (Succ n)

unsafeGetPattern :: Integral i
                 => Variants ps n
                 -> i
                 -> (forall p . Pattern p -> r)
                 -> r
unsafeGetPattern Empty              _ _ = error "Bounds violation"
unsafeGetPattern (Variant p _)      0 f = f p
unsafeGetPattern (Variant _ s)      n f = unsafeGetPattern s (n-1) f

mapVariants :: Variants ps n -> (forall p . Pattern p -> r) -> [r]
mapVariants Empty          _ = []
mapVariants (Variant p ps) f = f p : mapVariants ps f
