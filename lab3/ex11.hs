concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

-- [i | i <- xs1 ++ xs2]

concat'' :: [[a]] -> [a]
concat'' [i | i <- xs1 ++ xs2]

-- concatMap (\x -> x ++ "!") ["Ready", "Steady", "Go"]