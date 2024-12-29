module Main (main) where

import Test.QuickCheck
import Test.QuickCheck.Gen
--import Distribution.TestSuite.QuickCheck
--import qualified MyLib
import MyLib
import Cube
import Moves

main :: IO ()
--main = putStrLn "Test suite not yet implemented."
main = do 
    quickCheck asociatividad
    quickCheck neutro1
    quickCheck neutro2
    --quickCheck inverso1
    --quickCheck inverso2

--pruebas de propiedad de grupo (asociativo, neutro, inverso)

asociatividad :: Cube -> Cube -> Cube -> Property
asociatividad c1 c2 c3 = (c1 <> (c2 <> c3)) === ((c1 <> c2) <> c3)

neutro1 :: Cube -> Property
neutro1 c = (c <> mempty) === c

neutro2 :: Cube -> Property
neutro2 c = (mempty <> c) === c

--inverso1 :: Cube -> Property
--inverso1 c = (c <> (invert c)) === mempty

--inverso2 :: Cube -> Property
--inverso2 c = ((invert c) <> c) === mempty


{-
PROBAR
conds de grupo acíclico de cada mov (conmutatividad, 4 veces da resuelto)
algs que den resuelto

props matemáticas de grupos (orden)
-}
{-
probar en moves:
props de grupo (asociatividad, neutro [], inverso)
al simplificar da long <=

-}




instance Arbitrary Cube where
    arbitrary = do
        xs <- shuffle [0..53]
        return (C xs)
        --son permutaciones de stickers aleatorios. Por mejorar con piezas reales.

--instance Arbitrary BasicMove where
--    arbitrary = elements [ N .. ]
--
--instance Arbitrary Move where
--    arbitrary = elements listPossibleMoves
--Esto podría funcionar
--instance Arbitrary Algorithm where
--    arbitrary = listOf elements ((\x -> Alg [x]) listPossibleMoves)
--        Alg $ do
--
--            giro <- arbitrary
--            num <- choose (0, 3)
--            xs <- Alg (listOf (giro, num))
--            return (foldl (<>) [] xs )

        
  
  {-
          -- Generamos una lista aleatoria de tuplas [(Color, Int)]
        list1 <- arbitrary :: Gen [(Color, Int)]
        list2 <- arbitrary :: Gen [(Color, Int)]
        
        -- Combinamos las listas usando nuestro operador <>
        let combined = list1 <> list2

        -- Devolvemos un valor de tipo Algorithm
        return (Alg combined)-}        

--    genListaTuplas = listOf $ do
  --  color <- arbitrary  -- Usa la instancia de Arbitrary para Color
    --numero <- choose (1, 10) -- Genera números entre 1 y 10
    --return (color, numero)