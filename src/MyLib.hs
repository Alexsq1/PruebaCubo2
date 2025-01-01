module MyLib (someFunc) where
--import Cube
import Moves
import AlgToCube

someFunc :: IO ()
someFunc = do
    putStrLn "Probando src/MyLib"
    putStrLn "Haciendo un moveToPerms de algorithm1"
    putStrLn ("algorithm1 = " ++ (show algorithm1))


    where 
        strAlg = "F8 R2 U2 F' R L R L R L R  L D2 D L F2 F2 F2 " 
        algorithm1 = read strAlg :: Algorithm
