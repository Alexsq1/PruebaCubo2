module Moves where

import Data.Group
import Data.List
--import Utils

{-
Este módulo implementa el dato Algorithm,
que son las secuencias de movimientos
que se pueden aplicar (en notación Singmaster)
Ejemplo: R U R' U' F2 D2 B2 L

Tiene como datos "auxiliares" giros básicos (las capas que existe),
Move (un giro)
Algorithm se implementa como una lista de giros.
Aquí se implementa su IO, 
sus operaciones (concatenar algoritmos simplificando adyacentes y paralelos)
y propiedades (grupo).


-}

data BasicMove = N | R | U | F | L | D | B deriving (Show, Eq, Read, Enum)
--N es neutro, no hacer ningún giro
--añadir giros de capas intermedias y rotaciones?

newtype Move = M (BasicMove, Int) deriving (Eq)
data Algorithm = Alg [Move] deriving (Eq)

data Axis = NN | UD | RL | FB deriving (Show, Eq, Enum)

-------------------------------------------------------------

--CODIFICACIÓN DE 1 GIRO

{-
Read simplifica automáticamente:
Ej: R8 -> ""
    R5 -> R
    R7 -> R'
    R R R R -> ""
    R L R L -> R2 L2
    R U U' R' -> ""
    
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



--Codificación de operaciones de 1 Move:
{-
Tiene una estructura curiosa.
Tiene elemento neutro e inverso, 
pero no siempre hay operación binaria interna
Ej: R R -> R2
Pero R U -> R U, y son 2 giros y no uno
-}

--operación binaria no interna. 
--Si 2 moves cancelan, devuelve []
--Las N se las salta

twoMoves :: Move -> Move -> [Move]
twoMoves (M mov1) (M mov2)
    | (m1 == N) && (m2 == N)= []
    | m1 == N = [sm2]
    | m2 == N = [sm1]
    | (m1 == m2) = 
        let newMove = simp1Move (M(m1, n1 + n2))
        in
            if (isNull newMove) 
                then [] 
                else [newMove]
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

isNull :: Move -> Bool
isNull m = newMove == N
    where 
        M(newMove, _) = simp1Move m


recupBM :: Move -> BasicMove
recupBM (M(m,_)) = m

--Operaciones de axis




axisBM :: BasicMove -> Axis
axisBM m
    | m == U || m == D = UD
    | m == R || m == L = RL
    | m == F || m == B = FB
    | otherwise = NN

axis :: Move -> Axis
axis (M(m, _)) = axisBM m

axisOrd :: Axis -> BasicMove -> Ordering
axisOrd ax move
    | (ax == NN) && (move == N) = LT
    | (ax == RL) && (move == R) = LT
    | (ax == RL) && (move == L) = GT
    | (ax == UD) && (move == U) = LT
    | (ax == UD) && (move == D) = GT
    | (ax == FB) && (move == F) = LT
    | (ax == FB) && (move == B) = GT
    | otherwise = LT



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
staticReadAlg (_:_) = error "Canonicalizar falló, no longitud par"

--Usar <> porque va simplificando.

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

--Auxiliares de canonicalizar
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
    Alg (a1) <> Alg (a2) = Alg(simpFullAlg (a1 ++ a2))
--desde Read es O(n). Junta y luego simplifica.
--Cuidado: simpAdj usa init-last, puede que sea O(n^2)

--Simplificaciones de algoritmos (adyacentes y paralelos)
simpFullAlg :: [Move] -> [Move]
simpFullAlg = (simpParalels . simpAdj)


simpAdj :: [Move] -> [Move]
simpAdj = foldl myFunctionConcat []
    where 
        myFunctionConcat = 
            (\xs a -> 
                if (null xs)
                    then [a]
                else
                    ((init xs) ++ (twoMoves (last xs) a))
            )

--En cada fold, simplifica el último de lo que tiene con el nuevo
--Cuidado: si lista vacía en el momento no hay "último giro" (if-else)
--ideal: cancelar movs opuestos (R2 L2 R2 L2 -> []), pero puede que sea más complicado de lo que parece



simpParalels :: [Move] -> [Move]
simpParalels [] = []
simpParalels (x:xs) = (simpSubListSameAxis simplificable) ++ (simpParalels rest)
    where
        (simplificable, rest) = span (\y -> axis y == axis x) (x:xs)

        ----podría ser útil función splitWhen de Data.List.Split


----Pre: sabemos que tienen el mismo axis y que no es vacía
simpSubListSameAxis :: [Move] -> [Move]
simpSubListSameAxis [] = []
simpSubListSameAxis (x:xs)
    | (axisOrd (axis x) (recupBM x) ) == LT = simplfs1 ++ simplfs2
    | otherwise = simplfs2 ++ simplfs1
    where 
        (xs1, xs2) = partition (\y -> (recupBM y) == (recupBM x)) (x:xs)
        simplfs1 = simpAdj xs1
        simplfs2 = simpAdj xs2


irreducible :: Algorithm -> Bool
irreducible (Alg xs) = (length xs) == (length (simpFullAlg xs))

instance Monoid Algorithm where
    mempty = Alg []

instance Group Algorithm where
    invert (Alg xs) = Alg(invMovs xs)

invMovs :: [Move] -> [Move]
invMovs [] = []
invMovs (x:xs) = invMovs (xs) ++ [invOneMov x]
--Puede ser útil hacer alg divide&conquer, dividiendo +- a la mitad



