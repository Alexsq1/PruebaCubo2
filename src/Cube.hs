module Cube where

import Data.Group
import Data.List
import Utils
import Moves


--Definición y codificación de datos: Cubo como grupo

newtype Cube = C [Int] deriving (Show, Eq)
--Un cubo son 54 nums (0-53) usar base 64 en el futuro

instance Semigroup Cube where
    (C xs) <> (C ys) = C(perm xs ys)

instance Monoid Cube where
    mempty = C [0..53]
    --Cuidado: revisar según implementación

instance Group Cube where
    invert (C xs) = C(invert_perm xs)


--auxiliares de grupo

solved :: Cube -> Bool
solved = (== mempty)

--operador <> de Cubo, pero más genérico. Aplica a xs perm definida por ys 
perm :: [a] -> [Int] -> [a]
perm xs ys = map (\x -> xs !! x) ys

--Permutación inversa
invert_perm :: [Int] -> [Int]
invert_perm xs = map fst tups_ord
    where
        neutral = [0 .. (length xs - 1)]
        tups = zip (neutral) xs 
        tups_ord = sort_by_snd tups



--Definición de datos: piezas individuales 
--Con idea de permitir saber si una pieza está orientada, permutada, ambos (revisar)

data Piece = Edge | Corner | Center deriving (Show, Eq)

typeOfPiece :: Int -> Piece
typeOfPiece n
    | inRange n 0 23 = Corner
    | inRange n 24 47 = Edge
    | inRange n 48 53 = Center
    | otherwise = error "Valor no válido"

validPiece :: Int -> Bool
validPiece n = inRange n 0 53
    
pieceSolved :: Int -> Cube -> Bool
pieceSolved n (C xs) = n == (xs !! n)

numberOfPiece :: Piece -> Int -> Int
numberOfPiece Corner n = 3 * n 
numberOfPiece Edge n = 24 + 2 * n
numberOfPiece Center n = 48 + n

pieceOriented :: Int -> Cube -> Bool
pieceOriented n (C xs)
    | tp == Corner = pInPlace `mod` 3 == n `mod` 3
    | tp == Edge = pInPlace `mod` 2 == n `mod` 2
    | otherwise = True
        where 
            tp = typeOfPiece n
            pInPlace = xs !! n


--Estado válido y resoluble
--Codificar bien, faltan muchas restricciones

validState :: Cube -> Bool
validState (C xs) = (uniqueElems xs) && 
                (isPermutation lCorners [0..23]) && 
                (isPermutation lEdges [24..47]) && 
                (isPermutation lCenters [48..53])
    where
        lCorners = adjSublist xs 0 23
        lEdges = adjSublist xs 24 47
        lCenters = adjSublist xs 48 53

--Desde algoritmo a permutación:

basicMoveToPerm :: BasicMove -> Cube

basicMoveToPerm R = C[0,1,2,3,4,5,11,9,10,13,14,12,17,15,16,7,8,6,18,19,20,21,22,23,24,25,26,27,34,35,30,31,32,33,42,43,28,29,38,39,40,41,36,37,44,45,46,47,48,49,50,51,52,53]
basicMoveToPerm U = C[9,10,11,0,1,2,3,4,5,6,7,8,12,13,14,15,16,17,18,19,20,21,22,23,30,31,24,25,26,27,28,29,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53]
basicMoveToPerm F = C[22,23,21,3,4,5,6,7,8,2,0,1,10,11,9,15,16,17,18,19,20,14,12,13,24,25,26,27,28,29,33,32,41,40,31,30,36,37,38,39,35,34,42,43,44,45,46,47,48,49,50,51,52,53]
basicMoveToPerm L = C[5,3,4,19,20,18,6,7,8,9,10,11,12,13,14,15,16,17,23,21,22,1,2,0,38,39,26,27,28,29,30,31,24,25,34,35,36,37,46,47,40,41,42,43,44,45,32,33,48,49,50,51,52,53]
basicMoveToPerm D = C[0,1,2,3,4,5,6,7,8,9,10,11,21,22,23,12,13,14,15,16,17,18,19,20,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,46,47,40,41,42,43,44,45,48,49,50,51,52,53]
basicMoveToPerm B = C[0,1,2,8,6,7,16,17,15,9,10,11,12,13,14,20,18,19,4,5,3,21,22,23,24,25,37,36,28,29,30,31,32,33,34,35,45,44,27,26,40,41,42,43,39,38,46,47,48,49,50,51,52,53]
--ver si tiene sentido ejecutar solo los simples o guardar las perms de todos los giros individuales
basicMoveToPerm x = error "Movimiento no existente"


moveToPerm :: Move -> Cube
moveToPerm (mov, num) = (xs !! num)
    where
        ini = basicMoveToPerm mov
        xs = iterate (\x -> x <> ini) mempty
        --revisar si tiene sentido basarse en giros simples o directamente apuntar todos

algToPerm :: Algorithm -> Cube
algToPerm (Alg xs) = foldr (<>) mempty (map moveToPerm xs)
