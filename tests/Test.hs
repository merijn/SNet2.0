{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Test where

import SNet
import Distribution.TestSuite

import Data.Set hiding (map)

foreign import ccall "id" c_id :: Ptr C'Handle -> CInt -> IO ()

idBox :: SNet
idBox = box pattern (Variant pattern Empty) c_id
  where pattern = Cons (Tag 1) Nil

baseTest :: [Record Data] -> SNet -> [Record Data] -> IO Progress
baseTest outRecs net inRecs = do
    result <- runSNetCustom (dummyIn inRecs) dummyOut net
    return $ if fromList outRecs == fromList result
                then Finished Pass
                else Finished (Fail "Results don't match!")

testSimple :: (Record Data -> Record Data)
           -> SNet
           -> [Record Data]
           -> IO Progress
testSimple f net recs = baseTest (map f recs) net recs

smokeTests :: Test
smokeTests = Group
  { groupName = "Smoke tests"
  , concurrently = True
  , groupTests = map Test
    [ TestInstance
      { run = testSimple id idBox input
      , name = "id"
      , tags = []
      , options = []
      , setOption = \_ _ -> Left "Not supported"
      }
    , TestInstance
      { run = testSimple id (idBox --> idBox) input
      , name = "serial"
      , tags = []
      , options = []
      , setOption = \_ _ -> Left "Not supported"
      }
    ]
  }
  where input = map read ["Rec {(fromList [(1,5)]) (fromList [])}"]

tests :: IO [Test]
tests = return
  [ smokeTests
  ]
