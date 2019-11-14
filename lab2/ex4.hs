isPalindrome :: [Char] -> Bool
isPalindrome s = (s==reverse(s))

getElemAtIdx :: [Int] -> Int -> Int
getElemAtIdx xs i = head(drop i xs)

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (h:tail) = if fromEnum(h)>fromEnum('H')
						then [toEnum(fromEnum(h)-fromEnum('h')+fromEnum('H'))::Char] ++ tail
						else [h] ++ tail