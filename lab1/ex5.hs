sgn :: Int -> Int
sgn n = if n < 0
       then -1
       else if n == 0
            then 0
            else 1

absInt :: Int -> Int
absInt n = if n < 0
	   then n*(-1)
	   else if n == 0
		then 0
		else n

min2Int :: (Int, Int) -> Int
min2Int (x,y) = if x < y
		then x
		else y

min3Int :: (Int, Int, Int) -> Int
min3Int (x,y,z) = if min2Int(x,y)<z
		  then min2Int(x,y)
		  else z

toLower::Char->Char
toLower a = if fromEnum(a)<fromEnum('a')
            then toEnum(fromEnum(a)-fromEnum('A')+fromEnum('a'))::Char
            else a
			
toUpper:: Char->Char
toUpper a = if fromEnum(a)>fromEnum('A')
	    then toEnum(fromEnum(a)-fromEnum('a')+fromEnum('A'))::Char
	    else a

isDigit :: Char -> Bool
isDigit a = if (fromEnum(a)>=fromEnum('0') && fromEnum(a)<=fromEnum('9'))
	    then True
	    else False

charToNum :: Char -> Int
charToNum a = fromEnum(a)

romanDigit :: Char -> String
romanDigit a = if a=='1' 
	       then "Jeden"
	       else if a=='2'
	       then "Dwa"
	       else if a=='3'
	       then "Trzy"
	       else if a=='4'
	       then "Cztery"
	       else if a=='5'
	       then "Piec"
	       else if a=='6'
	       then "Szesc"
	       else if a=='7'
	       then "Siedem"
	       else if a=='8'
	       then "Osiem"
	       else if a=='9'
	       then "Dziewięc"
	       else [a]	