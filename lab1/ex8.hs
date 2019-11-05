not' :: Bool -> Bool
not' b = case b of
          True  -> False
          False -> True

absInt :: Int -> Int
absInt n = case (n >= 0) of
   True -> n
   _    -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer a = case (a=="Love") of 
	True -> True
	False -> False

or' :: (Bool, Bool) -> Bool
or' (x,y) = case ((x,y)==(False,False)) of
	True -> True
	False -> False

and' :: (Bool, Bool) -> Bool
and' (x,y) = case ((x,y)==(True,True)) of
	True -> True
	False -> False

nand' :: (Bool, Bool) -> Bool
nand' (x,y) = case ((x,y)==(True,True)) of
	True -> False
	False -> True

xor' :: (Bool, Bool) -> Bool
xor' (x,y) = case (x/=y) of
	True -> True
	False -> False