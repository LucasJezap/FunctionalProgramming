fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
 
fib2 n = take n fibs 
 
sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' [x] = x
prod' (x:xs) = x*prod' xs

length' :: [a] -> Int
length' [x] = 1
length' (x:xs) = 1+length' xs

or' :: [Bool] -> Bool
or' [x] = (x==True) 
or' (x:xs) = (x || or' xs)

and' :: [Bool] -> Bool
and' [x] = (x==True)
and' (x:xs) = (x && and' xs)

elem' :: Eq a => a -> [a] -> Bool
elem' e [x] = (x==e)
elem' e (x:xs) = (x==e || elem' e xs)

doubleAll :: Num t => [t] -> [t]
doubleAll [x] = [2*x]
doubleAll (x:xs) = ([2*x] ++ doubleAll xs)

squareAll :: Num t => [t] -> [t]
squareAll [x] = [x*x]
squareAll (x:xs) = ([x*x] ++ squareAll xs)

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs) = (if x `mod` 2 == 0 then [x] else []) ++ selectEven xs


