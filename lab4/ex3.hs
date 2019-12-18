data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree deriving Show

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a) deriving Show


instance Eq a => Eq (BinTree a) where (==) (NodeBT n1 lt1 rt1) (NodeBT n2 lt2 rt2) = (n1==n2 && lt1==lt2 && rt1==rt2)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a |                   -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a) | Multiply (Expr a) (Expr a) | Divide (Expr a) (Expr a)

eval :: Fractional a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Multiply e1 e2) = eval e1 * eval e2
eval (Divide e1 e2) = eval e1 / eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Multiply e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"
show' (Divide e1 e2) = "(" ++ show' e1 ++ "/" ++ show' e2 ++ ")"


depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = max (1+depthOfBT lt) (1+depthOfBT rt)

flattenBT :: BinTree a -> [a]
flattenBT EmptyBT = []
flattenBT (NodeBT n lt rt) = (flattenBT lt) ++ [n] ++ (flattenBT rt)

flattenBT' :: BinTree a -> [a]
flattenBT' EmptyBT = []
flattenBT' (NodeBT n lt rt) = [n] ++ (flattenBT lt) ++ (flattenBT rt)

flattenBT'' :: BinTree a -> [a]
flattenBT'' EmptyBT = []
flattenBT'' (NodeBT n lt rt) = (flattenBT rt) ++ [n] ++ (flattenBT lt)

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = (NodeBT (f n) (mapBT f lt) (mapBT f rt))

insert :: Ord a => a -> BinTree a -> BinTree a
insert a EmptyBT = NodeBT a EmptyBT EmptyBT
insert a (NodeBT n lt rt) 
    | n >= a = NodeBT n (insert a lt) rt
    | n < a = NodeBT n lt (insert a rt)


list2BST :: Ord a => [a] -> BinTree a
list2BST [] = EmptyBT
list2BST (x:xs) = insert x (list2BST xs)

occurs :: Eq a => a -> BinTree a -> Int
occurs x EmptyBT = 0
occurs x (NodeBT n lt rt)
    | x==n = 1 + occurs x lt + occurs x rt
    | otherwise = occurs x lt + occurs x rt

elemOf :: Eq a => a -> BinTree a -> Bool
elemOf x EmptyBT = False
elemOf x (NodeBT n lt rt) = (x==n || elemOf x lt || elemOf x rt)

reflect :: BinTree a -> BinTree a 
reflect EmptyBT = EmptyBT
reflect (NodeBT n lt rt) = (NodeBT n (reflect rt) (reflect lt))

minElemOf :: Ord a => BinTree a -> a
minElemOf (NodeBT n EmptyBT _) = n
minElemOf (NodeBT n lt rt) = minElemOf lt

maxElemOf :: Ord a => BinTree a -> a
maxElemOf (NodeBT n _ EmptyBT) = n
maxElemOf (NodeBT n lt rt) = maxElemOf rt
