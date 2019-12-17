import Data.List

sortDesc :: Ord a => [a] -> [a]
sortDesc = (reverse . sort) 

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt f g xs = [f i | i<-xs] == [g i | i<-xs]

-- composeFunList :: [a -> a] -> (a -> a)
-- composeFunList 
