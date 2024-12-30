module Moves where

import Data.Group
import Utils

{-
Este módulo implementa el dato Algorithm,
que son las secuencias de movimientos
que se pueden aplicar (en notación Singmaster)
Ejemplo: R U R' U' F2 D2 B2 L
Tiene como datos "auxiliares" giros básicos (las capas que existe),
Move (un giro)
Algorithm se implementa como una lista de giros.
Aquí se implementa su IO, 
sus operaciones (concatenar algoritmos simplificando)
y propiedades (grupo).


-}

data BasicMove = N | R | U | F | L | D | B deriving (Show, Eq, Read, Enum)
--N es neutro, no hacer ningún giro
--añadir giros de capas intermedias y rotaciones?

newtype Move = M (BasicMove, Int) deriving (Eq)
data Algorithm = Alg [Move] deriving (Eq)



-------------------------------------------------------------

--CODIFICACIÓN DE 1 GIRO

{-
Read simplifica automáticamente:
Ej: R8 -> ""
    R5 -> R
    R7 -> R'
    R R R R -> ""
    
-}

-------------------------------------------------------------

instance Read Move where
    readsPrec _ (x:y:rest) = 
        let move = read [x] :: BasicMove
            num =  read [y] :: Int
        in [ ( simp1Move (M(move, num)) , rest) ]

instance Show Move where
    show (M(m, n)) = show m ++ showNum n
        where
            showNum n
                | n == 1 = " "
                | n == 2 = "2 "
                | n == 3 = "' "
                | otherwise = ""
    --voy a intentar usar los menos casos posibles, y hacer mejor el método Read


listPossibleMoves :: [Move]
listPossibleMoves = map simp1Move [M(m, n) | m <- [N ..], n <- [0 .. 3]]


--Codificación de operaciones de 1 Move:
{-
Tiene una estructura curiosa.
Tiene elemento neutro e inverso, 
pero no siempre hay operación binaria interna
Ej: R R -> R2
Pero R U -> R U, y son 2 giros y no uno
-}

--operación binaria no interna
twoMoves :: Move -> Move -> [Move]
twoMoves (M mov1) (M mov2)
    | m1 == N = [sm2]
    | m2 == N = [sm1]
    | m1 == m2 = [simp1Move (M(m1, n1 + n2))]
    | otherwise = [sm1, sm2]
    where
        sm1 = simp1Move (M mov1)
        sm2 = simp1Move (M mov2)
        M(m1, n1) = sm1
        M(m2, n2) = sm2
        
--Elemento inverso
invOneMov :: Move -> Move
invOneMov (M(m, n)) = simp1Move (M(m, (4-n)))

--Hace módulo
simp1Move :: Move -> Move
simp1Move (M(m, n))
    | ((n `mod` 4) == 0 || m == N) = M(N, 0)
    | otherwise = M(m, n `mod` 4)


------------------------------------
--CODIFICACIÓN ALGORITHM (VARIOS GIROS)
------------------------------------

lengthAlg :: Algorithm -> Int
lengthAlg (Alg a) = length a

instance Show Algorithm where
    show (Alg xs) = "[" ++ foldl (\x y -> x ++ (show y)) "" xs ++ "]"

instance Read Algorithm where
    readsPrec _ str = [(  (staticReadAlg (canonicalizar str)), [])]

--Un poco cutre, pero permite preprocesado y postsprocesado de cadena

--AUXILIARES DE READ DE MOVES:

--PRE: recibe una cadena canonicalizada
staticReadAlg :: String -> Algorithm
staticReadAlg "" = Alg[]
staticReadAlg (x:y:xs) = Alg[read ([x] ++ [y])] <> staticReadAlg xs

--Usar <> porque va simplificando. Creo que tiene O(n²)

--Canonicalizar convierte en (GN)*, Giro-número n veces
{-
Ejemplo:
R U F2 L' D2 B U'
R1U1F2L3D2B1U3
-}

canonicalizar :: String -> String
canonicalizar str = insertOnes strPrimas
    where
        strSinEspacios = filter (/= ' ') str
        strPrimas = map (\x -> if (x == '\'') then '3' else x) strSinEspacios

--Auxiliares de
insertOnes :: String -> String
insertOnes "" = ""
insertOnes (x:y:xs) = if ((isChrBM x && isChrBM y)) then ([x] ++ "1" ++ insertOnes(y:xs)) else ([x] ++ insertOnes (y:xs))
insertOnes (x:xs) = if (isChrBM x) then ([x] ++ "1" ++ insertOnes xs) else ([x] ++ insertOnes xs)

isChrBM :: Char -> Bool
isChrBM str = elem str (show [N .. ])


------------------------------------
--CODIFICACIÓN ALGORITHM COMO GRUPO
---------------------------------
instance Semigroup Algorithm where
    Alg (a1) <> Alg (a2) = Alg(simpAlg (a1 ++ a2))
--desde Read es O(n²) ?????
--Puede ser mejor idea ir concatenando uno por uno
--Alg divide and conquer
--Usar los ejes


simpAlg :: [Move] -> [Move]
simpAlg [] = []
simpAlg (x:xs) = simpAlgRec [] x xs


--Cuidado con tipos de Alg y [Moves]
simpAlgRec :: [Move] -> Move -> [Move] -> [Move]
simpAlgRec done current future
    | (future == [] && current == M(N, 0)) = done      --fin
    | future == [] = (done ++ [current])      --fin
    | (current == M(N, 0) && (null done))     = simpAlgRec done (head future) (tail future)
    | current == M(N, 0)                      = simpAlgRec (init done) (last done) future
    | ((length new == 1) && first_move /= N) = simpAlgRec done first_elem (tail future)     --cancelan parc
    | ((length new == 1) && first_move == N) = simpAlgRec done first_elem (tail future)                 --cancelación 100%
    | otherwise                              = simpAlgRec (done ++ [current]) (head future) (tail future)                 --no cancelan
    where    
        new = twoMoves current (head (future))
        first_elem = head new
        M(first_move, _) = first_elem
--ideal: cancelar movs opuestos (R2 L2 R2 L2 -> []), pero puede que sea más complicado de lo que parece


instance Monoid Algorithm where
    mempty = Alg []

instance Group Algorithm where
    invert (Alg xs) = Alg(invMovs xs)

invMovs :: [Move] -> [Move]
invMovs [] = []
invMovs (x:xs) = invMovs (xs) ++ [invOneMov x]
--Puede ser útil hacer alg divide&conquer, dividiendo +- a la mitad

