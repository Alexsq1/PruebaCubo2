module MyLib (someFunc) where
import Cube
import Moves

someFunc :: IO ()
someFunc = putStrLn $ show (pruebaPrint m)
    where m = R

pruebaPrint :: BasicMove -> Cube
pruebaPrint = basicMoveToPerm
