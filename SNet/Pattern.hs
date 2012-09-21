{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
module SNet.Pattern
  ( Pattern (..)
  , length
  , foldPattern
  , foldWithPattern
  ) where

import Prelude hiding (length)
import SNet.Types (RecEntry)

data Pattern :: [*] -> * where
  Nil :: Pattern '[]
  Cons :: RecEntry h -> Pattern t -> Pattern (h ': t)

length :: Integral i => Pattern p -> i
length Nil = 0
length (Cons _ t) = 1 + length t

foldPattern :: Pattern p -> (forall e . RecEntry e -> a -> a) -> a -> a
foldPattern Nil        _ base = base
foldPattern (Cons h t) f base = foldPattern t f (f h base)

foldWithPattern :: (forall e . RecEntry e -> val -> a -> a)
                -> a
                -> [val]
                -> Pattern p
                -> a
foldWithPattern _ r []     Nil         = r
foldWithPattern f r (v:vs) (Cons p ps) = foldWithPattern f (f p v r) vs ps
foldWithPattern _ _ _ _ = error "foldWithPattern: Pattern/list length mismatch!"
