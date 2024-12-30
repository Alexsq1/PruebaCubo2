module AlgToCube where

import Moves
import Cube






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


--moveToPerm :: Move -> Cube
--moveToPerm (mov, num) = (xs !! num)
--    where
--        ini = basicMoveToPerm mov
--        xs = iterate (\x -> x <> ini) mempty
--        --revisar si tiene sentido basarse en giros simples o directamente apuntar todos
--
--algToPerm :: Algorithm -> Cube
--algToPerm (Alg xs) = foldr (<>) mempty (map moveToPerm xs)
