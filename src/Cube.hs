module Cube where

import Data.Group
import Data.List
import Utils

{-
Este módulo implementa la permutación de un cubo,
su IO, sus operaciones (concatenar permutaciones)
y sus propiedades (grupo)
-}

newtype Cube = C [Int] deriving (Show, Eq)
--Un cubo son 54 nums (0-53) usar base 64 en el futuro


instance Semigroup Cube where
    (C xs) <> (C ys) = C(perm xs ys)

--operador <> de Cubo, pero más genérico. 
--Aplica a xs perm definida por ys 

perm :: [a] -> [Int] -> [a]
perm xs ys = map (\x -> xs !! x) ys




instance Monoid Cube where
    mempty = C [0..53]
    --Cuidado: revisar según implementación

--auxiliares de Cubo

solved :: Cube -> Bool
solved = (== mempty)
--Revisar si se permite cambiar la orientación





instance Group Cube where
    invert (C xs) = C(invert_perm xs)

--Permutación inversa
invert_perm :: [Int] -> [Int]
invert_perm xs = map fst tups_ord
    where
        neutral = [0 .. (length xs - 1)]
        tups = zip (neutral) xs 
        tups_ord = sort_by_snd tups






