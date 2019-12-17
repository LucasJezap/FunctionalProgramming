let f1 = \x -> x-2

let f2 = \x -> \y -> sqrt(x^2+y^2)

let f3 = (\x y z -> sqrt(fromIntegral(x^2+y^2+z^2))) :: Integer -> Integer -> Integer -> Double

(\x -> 2*x)
(\x -> x*2)
(\x -> 2^x)
(\x -> x^2)
(\x -> 2/x)
(\x -> x/3)
(\x -> 4-x)

(\x -> sqrt x)
(\x -> abs x)
(\x -> log x)
(\x -> id x)
(\x y -> const x y)

let f7 = \x -> if (even x) then True else False
let f8 = \x -> let y = sqrt x in 2*y^3*(y+1)
let f9 = \x -> if (x==1) then 3 else 0