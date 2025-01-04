module PBTGenMoves (testGenMoves) where

import Test.QuickCheck
import Moves
import GenMoves
import PBTMoves()
import Utils
--import Data.Group


testGenMoves :: IO ()
testGenMoves = do 
    quickCheck ( lengthGenMovesLayersInt)


lengthGenMovesLayersInt :: [BasicMove] -> [Int] -> Property
lengthGenMovesLayersInt layers nums = length (genMovesLayersInt layers nums)
                                    === (length layersProcessed) * (length numsProcessed)
                                    where 
                                        --bmsPost = ((\xs -> remove xs N ) . removeDups) bms
                                        layersProcessed = (removeDups . filter (/= N)) layers
                                        numsProcessed = (removeDups . filter (> 0) . map (flip mod 4)) nums 

