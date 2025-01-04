module GenMoves where

import Moves
import Utils

--Generar movimientos sueltos

genMovesLayersInt :: [BasicMove] -> [Int] -> [Move]
genMovesLayersInt layers nums = [(simp1Move (M(m, n))) | m <- layersPost, n <- numsPost]
    where
        layersPost = (removeDups . filter (/= N)) layers
        numsPost = (removeDups . filter (> 0) . map (flip mod 4)) nums 

listPossibleMoves :: [Move]
listPossibleMoves = genMovesLayersInt [N .. ] [1 .. 3]

listDoubleMoves :: [Move]
listDoubleMoves = genMovesLayersInt [N .. ] [2]


--Generar algoritmos de longitud n
--CUIDADO: O(18 ^ n), tarda para n = 4

genAlgs :: Int -> [BasicMove] -> [Int] -> [Algorithm]
genAlgs 0 _ _ = []
genAlgs 1 bms nums = map (\x -> Alg [x]) (genMovesLayersInt bms nums)
genAlgs long bms nums = clean xs
    where 
        xs = [(Alg [x]) <> y | x <- singles, y <- subalgs]
        singles = genMovesLayersInt bms nums
        subalgs = genAlgs (long - 1) bms nums
        clean = removeDups . filter (\x -> lengthAlg x == long)



--Sería muy bonito hacer prop: 
--length (genAlgs x [N .. ] [1 .. 3]) == cardAlgs x
--Pero tardaría demasiado

cardAlgs :: Integer -> Integer
cardAlgs 0 = 1
cardAlgs 1 = 18
cardAlgs n = round (18 * fibCustom (n - 1) 1 1 (27 / 2))
    where
        fibCustom :: Fractional a => Integer -> Integer -> a -> a -> a
        fibCustom num it a0 a1
            | num < 0 = 1
            | it == num = a1
            | otherwise = fibCustom num (it + 1) a1 (18 * a0 + 12 * a1)

