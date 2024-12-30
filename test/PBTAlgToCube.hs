module PBTAlgToCube (testAlgToCube) where

import Test.QuickCheck
import Cube
import Moves
import AlgToCube
import PBTMoves

--import Data.Group

testAlgToCube :: IO()
testAlgToCube = do
    quickCheck ciclo4

ciclo4 :: Move -> Bool
ciclo4 m = solved (perm <> perm <> perm <> perm)
    where perm = moveToPerm m


{-Probar "isomorfismo" entre permutaciones y giros -}


