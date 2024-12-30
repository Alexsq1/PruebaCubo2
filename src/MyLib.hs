module MyLib (someFunc) where
--import Cube
import Moves
--import AlgToCube

someFunc :: IO ()
someFunc = do
    putStrLn "Probando src/MyLib"
    putStrLn "Haciendo un read de :"
    putStrLn "R2 U2 F' D2 L F2"
    putStrLn (show xs)

    where xs = read "R2 U2 F'  R R R R    D2  L   F2 F2 F2 " :: Algorithm
