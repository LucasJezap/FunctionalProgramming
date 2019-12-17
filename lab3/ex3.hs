sqr x = x^2

funcFactory n = case n of
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5

expApproxUpTo :: Int -> Double -> Double 
expApproxUpTo n x = until 0 n 0 x
    where
        until k n acc x
            | k > n = acc
            | otherwise = until (k + 1) n (acc + (calc x k)) x
        
        calc x k = (x ^^ k) / (fromIntegral (product [1..k]))

dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = f h