fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fstDivSec :: [Int] -> Bool
fstDivSec (x:y:_) | (y `mod` x == 0)       = True
fstDivSec _ 			       = False	

fstDivThird :: [Int] -> Bool
fstDivThird (x:y:z:_) | (z `mod` x == 0) = True
fstDivThird _ 				 = False