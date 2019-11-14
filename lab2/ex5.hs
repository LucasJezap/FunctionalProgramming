-- length [(a,b,c) | a <- [1..100], b <-[a..100], c <-[b..100], a^2+b^2==c^2]

isPrime :: Integral t => t -> Bool
isPrime 1 = False
isPrime n = [i | i <-[2..n-1], n `mod` i == 0] == []

-- niepoprawna bo dla 1 daje prawde, nieefektywna, można do pierwiastka z n 

-- length [i | i <- [1..10000], isPrime(i)==True]

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve [] = []
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]


isPrime' n = n `elem` (take n primes)

allEqual :: Eq a => [a] -> Bool
allEqual (p:xs) = [i | i <- xs, i /= p] == []