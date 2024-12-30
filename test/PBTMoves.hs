module PBTMoves (testMoves) where

import Test.QuickCheck
import Moves
import Data.Group


testMoves :: IO ()
testMoves = do 
    quickCheck ( invOneMoveProp1)
    quickCheck ( invOneMoveProp2)
    quickCheck ( asociatividad)
    quickCheck ( neutro1)
    quickCheck ( neutro2)    
    quickCheck ( inverso1)
    quickCheck ( inverso2)



invOneMoveProp1 :: Move -> Property
invOneMoveProp1 m = (length (twoMoves m (invOneMov m)) === 1)

invOneMoveProp2 :: Move -> Property
invOneMoveProp2 m = (head (twoMoves m (invOneMov m)) === M(N,0))


asociatividad :: Algorithm -> Algorithm -> Algorithm -> Property
asociatividad a1 a2 a3 = (a1 <> (a2 <> a3)) === ((a1 <> a2) <> a3)

neutro1 :: Algorithm -> Property
neutro1 a = (a <> mempty) === a

neutro2 :: Algorithm -> Property
neutro2 a = (mempty <> a) === a

inverso1 :: Algorithm -> Property
inverso1 a = (a <> (invert a)) === mempty

inverso2 :: Algorithm -> Property
inverso2 a = ((invert a) <> a) === mempty


{-
probar en moves:
props de grupo (asociatividad, neutro [], inverso)
al simplificar da long <=
-}


instance Arbitrary BasicMove where
    arbitrary = elements [ N .. ]

instance Arbitrary Move where
    arbitrary = elements listPossibleMoves

instance Arbitrary Algorithm where
    arbitrary = do 
        xs <- listOf arbitrary
        return (Alg (mempty) <> Alg (xs))