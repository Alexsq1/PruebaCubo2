module Main (main) where


import PBTCube
import PBTMoves
import PBTAlgToCube
import PBTGenMoves


main :: IO ()
--main = putStrLn "Test suite not yet implemented."
main = do 
  putStrLn "Running tests of Cube"
  testCube

  putStrLn "Running tests of Moves"
  testMoves

  putStrLn "Running tests of AlgToCube"
  testAlgToCube

  putStrLn "Running tests of GenMoves"
  testGenMoves
