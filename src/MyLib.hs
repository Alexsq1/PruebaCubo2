module MyLib (someFunc) where
--import Cube
import Moves
import GenMoves
import AlgToCube
import Solver

someFunc :: IO ()
someFunc = do
    putStrLn "Probando src/MyLib"
    --putStrLn "Haciendo un moveToPerms de algorithm1"
    --putStrLn ("algorithm1 = " ++ (show algorithm1))
    --putStrLn "Generando moves: "
    --putStrLn (show (genMovesLayersInt [R, R, U] [1, 2, 3]))
    --putStrLn (show (listPossibleMoves))
    --putStrLn (show (listDoubleMoves))
    putStrLn (show (iddfs (algToPerm algorithm1)))
    putStrLn (show (iddfs (algToPerm algorithm2)))
    putStrLn (show (iddfs (algToPerm algorithm3)))



    where 
        --strAlg = "F8 R2 L2 R2 U2 F' R L R L R L R  L D2 D L F2 F2 F2 " 
        strAlg1 = "R2 U2 F2 L'" 
        algorithm1 = read strAlg1 :: Algorithm
        
        strAlg2 = "R2 U2 F2 L' R" -- Inmediato
        algorithm2 = read strAlg2 :: Algorithm
        
        strAlg3 = "R2 U2 F2 L' R D2" --1 minuto aprox
        algorithm3 = read strAlg3 :: Algorithm
        
