module MyLib (someFunc) where
--import Cube
import Moves
import GenMoves
--import AlgToCube

someFunc :: IO ()
someFunc = do
    putStrLn "Probando src/MyLib"
    putStrLn "Haciendo un moveToPerms de algorithm1"
    putStrLn ("algorithm1 = " ++ (show algorithm1))
    putStrLn "Generando moves: "
    putStrLn (show (genMovesLayersInt [R, R, U] [1, 2, 3]))
    putStrLn (show (listPossibleMoves))
    putStrLn (show (listDoubleMoves))
    


    where 
        strAlg = "F8 R2 L2 R2 U2 F' R L R L R L R  L D2 D L F2 F2 F2 " 
        algorithm1 = read strAlg :: Algorithm
