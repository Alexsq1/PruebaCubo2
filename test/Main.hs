module Main (main) where


import PBTCube
import PBTMoves
import PBTAlgToCube


main :: IO ()
--main = putStrLn "Test suite not yet implemented."
main = do 
  putStrLn "Running tests of Cube"
  testCube

  putStrLn "Running tests of Moves"
  testMoves

  putStrLn "Running tests of AlgToCube"
  testAlgToCube

