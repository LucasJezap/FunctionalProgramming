isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = foldl (\acc (a,b) -> and [acc, a<=b]) True (zip xs (tail xs))

everySecond :: [t] -> [t]
everySecond xs = 