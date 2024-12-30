module PBTAlgToCube (testAlgToCube) where

import Test.QuickCheck
import Cube
import Moves
import AlgToCube
import PBTMoves     --incluye Arbitrary Algorithm

--import Data.Group

testAlgToCube :: IO()
testAlgToCube = do
    quickCheck ( ciclo4)
    quickCheck ( isomorfismo)

ciclo4 :: Move -> Bool
ciclo4 m = solved (permObt <> permObt <> permObt <> permObt)
    where permObt = moveToPerm m
--Mejorar según giros dobles y capas (ValidCube)


isomorfismo :: Algorithm -> Algorithm -> Property
isomorfismo alg1 alg2 = algToPerm (alg1 <> alg2)
                        === (algToPerm alg1) <> (algToPerm alg2)

{-Significa que concatenar los movimientos y luego generar
la permutación equivale a generar las permutaciones de 
los algoritmos individualmente y al final concatenarlas
-}




{-Probar secuencias de giros que dan resuelto y su permutación-}

