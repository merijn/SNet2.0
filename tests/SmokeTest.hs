{-# LANGUAGE GADTs #-}
module Main where

import SNet

foreign import ccall "id" c_id :: Ptr C'Handle -> CInt -> IO ()

net :: SNet
net = box pattern (Variant pattern Empty) c_id
  where pattern = Cons (Tag 1) Nil

main :: IO ()
main = runSNetCustom (dummyIn input) dummyOut net >>= print
  where input = map read ["Rec {(fromList [(1,5)]) (fromList [])}"]
