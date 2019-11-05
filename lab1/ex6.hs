absInt :: Int -> Int
absInt n | n >= 0    = n
         | otherwise = -n

sgn :: Int -> Int
sgn n | n < 0 = -1
      | n == 0 = 0
      | otherwise = 1

min3Int :: (Int, Int, Int) -> Int
min3Int (x,y,z) | (x<y && x<z) = x
		| (y<z) = y
		| otherwise = z

toUpper:: Char->Char
toUpper a | (fromEnum(a)>fromEnum('A')) = toEnum(fromEnum(a)-fromEnum('a')+fromEnum('A'))::Char
	  | otherwise = a
	   

toLower:: Char->Char
toLower a | (fromEnum(a)<fromEnum('a')) = toEnum(fromEnum(a)-fromEnum('A')+fromEnum('a'))::Char
          | otherwise = a 

isDigit :: Char -> Bool
isDigit a | (fromEnum(a)>=fromEnum('0') && fromEnum(a)<=fromEnum('9')) = True
	  | otherwise = False

charToNum :: Char -> Int
charToNum a = fromEnum(a)

romanDigit :: Char -> String
romanDigit a |  a=='1' = "Jeden" 
	     |  a=='2' = "Dwa"
	     |  a=='3' = "Trzy"
	     |  a=='4' = "Cztery"
	     |  a=='5' = "Piec"
	     |  a=='6' = "Szesc"
	     |  a=='7' = "Siedem"
	     |  a=='8' = "Osiem"
	     |  a=='9' = "Dziewięc"
	     |  otherwise = [a]	