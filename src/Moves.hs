module Moves where

import Data.Group
import Utils

data BasicMove = N | R | U | F | L | D | B deriving (Show, Eq, Read, Enum)
--N es neutro, no hacer ningún giro
--añadir giros de capas intermedias y rotaciones?

type Move = (BasicMove, Int)
data Algorithm = Alg [Move] deriving (Eq)

instance Show Algorithm where
    show (Alg xs) = if (null xs) then "[]" else showCutreMovs xs

showCutreMovs :: [Move] -> String
showCutreMovs [] = ""
showCutreMovs (x:xs) = (show mov) ++ (showCutreNum n) ++ " " ++ (showCutreMovs xs)
    where
        (mov, n) = x
        showCutreNum n
            | n == 2 = "2"
            | n == 3 = "'"
            | otherwise = ""

--buscar preprocesado de String, y que Read haga uso

preprocessString :: String -> String
preprocessString xs = toCanonicAlg primes
    where primes = (map (\x -> if x == '\'' then '3' else x) xs) ++ " "

toCanonicAlg :: String -> String
toCanonicAlg [] = []
toCanonicAlg (x:y:xs) 
    | (isChrBM x) && (y == ' ') = [x] ++ "1 " ++ toCanonicAlg xs
    | (x == ' ') && (y == ' ') = toCanonicAlg (y:xs)
    | otherwise = [x] ++ toCanonicAlg (y:xs)
toCanonicAlg (x:xs)
    | (isChrBM x) = [x] ++ "1 " ++ toCanonicAlg xs
    | otherwise = toCanonicAlg (xs)

isChrBM :: Char -> Bool
isChrBM str = member (show [N .. ]) str

correctMod :: Char -> Char
correctMod ch 
    | ch == '\'' = '3'
    | ch == '2' = '2'
    | ch == '1' = '1'
    | otherwise = '0'

{-
instance Read Move where
    
    readsPrec _ input =
        case reads input :: [((BasicMove, Char), String)] of
            [(mov, resto)] -> [(mov,  resto)]
            _                -> []
No tengo ni idea de cómo implementar el Read para un algoritmo
-}



instance Semigroup Algorithm where
    Alg (a1) <> Alg (a2) = Alg(simpAlg (a1 ++ a2))

instance Monoid Algorithm where
    mempty = Alg []

instance Group Algorithm where
    invert (Alg xs) = Alg(invMovs xs)



twoMoves :: Move -> Move -> [Move]
twoMoves mov1 mov2
    | m1 == N = [sm2]
    | m2 == N = [sm1]
    | m1 == m2 = [simp1Move (m1, n1 + n2)]
    | otherwise = [sm1, sm2]
    where
        sm1 = simp1Move mov1
        sm2 = simp1Move mov2
        (m1, n1) = sm1
        (m2, n2) = sm2
        

simp1Move :: Move -> Move
simp1Move (m, n) 
    | ((n `mod` 4) == 0 || m == N) = (N, 0)
    | otherwise = (m, n `mod` 4)


simpAlg :: [Move] -> [Move]
simpAlg [] = []
simpAlg (x:xs) = simpAlgRec [] x xs



simpAlgRec :: [Move] -> Move -> [Move] -> [Move]
simpAlgRec done current future
    | (future == [] && current == (N, 0)) = done      --fin
    | future == [] = (done ++ [current])      --fin
    | (current == (N, 0) && (null done))     = simpAlgRec done (head future) (tail future)
    | current == (N, 0)                      = simpAlgRec (init done) (last done) future
    | ((length new == 1) && first_move /= N) = simpAlgRec done first_elem (tail future)     --cancelan parc
    | ((length new == 1) && first_move == N) = simpAlgRec done first_elem (tail future)                 --cancelación 100%
    | otherwise                              = simpAlgRec (done ++ [current]) (head future) (tail future)                 --no cancelan
    where    
        new = twoMoves current (head (future))
        first_elem = head new
        first_move = fst first_elem
--ideal: cancelar movs opuestos (R2 L2 R2 L2 -> [])

invOneMov :: Move -> Move
invOneMov (m, n) = simp1Move(m, (4-n))

invMovs :: [Move] -> [Move]
invMovs [] = []
invMovs (x:xs) = invMovs (xs) ++ [invOneMov x]



