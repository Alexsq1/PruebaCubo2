module PBTCube (testCube) where

import Test.QuickCheck
--import Test.QuickCheck.Gen
import Cube
import Data.Group


testCube :: IO ()
testCube = do 
    quickCheck ( asociatividad)
    quickCheck ( neutro1)
    quickCheck ( neutro2)
    quickCheck ( inverso1)
    quickCheck ( inverso2)
    quickCheck (inversodoble)

--pruebas de propiedad de grupo (asociativo, neutro, inverso)

asociatividad :: Cube -> Cube -> Cube -> Property
asociatividad c1 c2 c3 = (c1 <> (c2 <> c3)) === ((c1 <> c2) <> c3)

neutro1 :: Cube -> Property
neutro1 c = (c <> mempty) === c

neutro2 :: Cube -> Property
neutro2 c = (mempty <> c) === c

inverso1 :: Cube -> Property
inverso1 c = (c <> (invert c)) === mempty

inverso2 :: Cube -> Property
inverso2 c = ((invert c) <> c) === mempty

inversodoble :: Cube -> Property
inversodoble c = ( (invert . invert) c) === c



{-
PROBAR
props matemáticas de grupos (orden), algoritmos que resuelven,
...
-}




instance Arbitrary Cube where
    arbitrary = do
        xs <- shuffle [0..53]
        return (C xs)
        --son permutaciones de stickers aleatorios. Por mejorar con piezas reales.

