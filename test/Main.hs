module Main (main) where


import PBTCube
import PBTMoves
import PBTAlgToCube


main :: IO ()
--main = putStrLn "Test suite not yet implemented."
main = do 
  testCube
  testMoves
  testAlgToCube

