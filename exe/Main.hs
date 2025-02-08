module Main where

import qualified MyLib (someFunc)
--import Moves

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
