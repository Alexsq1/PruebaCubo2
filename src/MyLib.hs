module MyLib (someFunc) where
--import Cube
import Moves
import AlgToCube

someFunc :: IO ()
someFunc = do
    putStrLn "Probando src/MyLib"
    putStrLn "Haciendo un moveToPerms de xs"
    putStrLn ("xs = " ++ (show xs))
    putStrLn $ show (algToPerm xs)

    where xs = read "R2 U2 F' R R R R  D2 D L F2 F2 F2 " :: Algorithm
