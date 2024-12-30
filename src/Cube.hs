module Cube where

import Data.Group
import Data.List
import Utils
--import Moves


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
--Con idea de permitir saber si una pieza está orientada, permutada, ambos 
--No terminado, (revisar)

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
