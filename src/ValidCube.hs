module ValidCube where

import Cube
import Utils


{-
Este módulo no está terminado.
La idea es saber si los estados de un cubo
son válidos, resolubles, posibles...

También estaría bien hacer una "implementación"
por tipos de piezas, núm (para facilitar unicidad)
que utilice a Cube. Permitiría saber si las piezas
están en su sitio, orientadas o ambas.
-}

--Definición de datos: piezas individuales 
--Con idea de permitir saber si una pieza está orientada, permutada, ambos 
--No terminado, (revisar)


--Añadir ejes (LR, UD, FB). 
--Utilidad para cancelación LRLR
--y probar órdenes de algoritmos

--Hacer método de generación:
{-
[R, U] -> [R R' R2 U U' U2]
" CON DOBLES -> [R2, U2]
(Con listas por comprensión sería fácil)
-}


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
