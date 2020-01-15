{-# LANGUAGE DeriveFunctor #-}

newtype Box a = MkBox a deriving (Show, Functor)

data MyList a = EmptyList
              | Cons a (MyList a) deriving (Show, Functor)


data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show, Functor)

-- instance Functor BinTree where
--     fmap _ EmptyBT = EmptyBT
--     fmap f (NodeBT a lt rt) = (NodeBT (f a) (fmap f lt) (fmap f rt))

newtype Pair b a = Pair { getPair :: (a,b) } deriving (Show, Functor) -- fmap should change the first element

-- instance Functor (Pair b) where
--     fmap f (Pair a) = (Pair (f a) b)

-- data Tree2 a = EmptyT2 | Leaf a | Node (Tree2 a) a (Tree2 a) deriving Show

-- instance Functor Tree2 where
--     fmap _ EmptyT2 = EmptyT2
--     fmap f (Leaf a) = Leaf (f a)
--     fmap f (Node lt a rt) = (Node (fmap f lt) (f a) (fmap f rt))

data GTree a = Leaf a | GNode [GTree a] deriving (Show, Functor)

-- instance Functor GTree where
--     fmap _ [] = []
--     fmap f (Leaf a) = Leaf (f a)
--     fmap f (GNode (r:rt)) = GNode ([f r] ++ (fmap f rt)) 


