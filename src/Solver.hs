module Solver where

import Cube
import GenMoves
import Moves
import AlgToCube
import Data.Maybe

--Cuidado: puede no acabar
iddfs :: Cube -> Maybe[Move]
iddfs = iterDeep 0
    where
        iterDeep :: Int -> Cube -> Maybe[Move]
        iterDeep 20 _ = Nothing
        iterDeep n c 
            | isNothing currSearch = iterDeep (n+1) c
            | otherwise = Just(fromJust (currSearch))
            where
                currSearch = boundedDFS mempty c n


boundedDFS :: Cube -> Cube -> Int -> Maybe [Move]
boundedDFS end start bound = auxDFSSgle end start 0 bound []
    
auxDFSSgle :: Cube -> Cube -> Int -> Int -> [Cube] -> Maybe [Move]
auxDFSSgle end start currentDepth maxBound visited
    | currentDepth > maxBound || currentDepth > 20 = Nothing
    | start `elem` visited = Nothing
    | end == start = Just []
    | otherwise = auxDFSMult end start listPossibleMoves currentDepth maxBound (start: visited)
    --where
        --nextLayer = map (\x -> start <> (moveToPerm x)) listPossibleMoves

auxDFSMult :: Cube -> Cube -> [Move] -> Int -> Int -> [Cube] -> Maybe [Move]
auxDFSMult end start [] currentDepth maxBound visited = Nothing
auxDFSMult end start (x:xs) currentDepth maxBound visited
    | isNothing thisBrach = auxDFSMult end start xs currentDepth maxBound visited
    | otherwise = Just (x : (fromJust thisBrach))
    where
        thisBrach = auxDFSSgle end (start <> (moveToPerm x)) (currentDepth + 1) maxBound (visited)

--Ideas: compartir mejor el set de visitados
--Mejorar visited con un set


--inicio, final, límite
