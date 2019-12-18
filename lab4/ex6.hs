class Mappable t where
    fMap :: (a -> b) -> t a -> t b
  
data Vec3D a = Vec3D {x::a, y::a, z::a} deriving (Show, Eq)
  
instance Mappable Vec3D where
    fMap f (Vec3D x y z) = Vec3D (f x) (f y) (f z)


newtype Pair a = Pair (a,a) deriving Show

instance Mappable Pair where
    fMap f (Pair (x,y)) = Pair (f x, f y)

data BinTree a = EmptyBT |
    NodeBT a (BinTree a) (BinTree a)
    deriving Show

instance Mappable BinTree where
    fMap f (EmptyBT) = EmptyBT
    fMap f (NodeBT n lt rt) = NodeBT (f n) (fMap f lt) (fMap f rt)
	
instance Mappable Maybe where
	fMap f (Nothing) = Nothing
	fMap f (Just a) = Just (f a)


class VectorLike t where
 (|==|) :: Eq a => t a -> t a -> Bool
 (|+|), (|-|) :: (Num a) => t a -> t a -> t a
 (|*|) :: (Num a) => t a -> t a -> a
 (||?), (|-?) :: (Num a, Eq a) => t a -> t a -> Bool -- równoległość i prostopadłość

instance VectorLike (Vec3D)  where
	(|==|) (Vec3D {x=a1,y=b1,z=c1}) (Vec3D {x=a2,y=b2,z=c2}) = (a1==a2 && b1==b2 && c1==c2)
	(|+|) (Vec3D {x=a1,y=b1,z=c1}) (Vec3D {x=a2,y=b2,z=c2}) = Vec3D {x=a1+a2,y=b1+b2,z=c1+c2}
	(|-|) (Vec3D {x=a1,y=b1,z=c1}) (Vec3D {x=a2,y=b2,z=c2}) = Vec3D {x=a1-a2,y=b1-b2,z=c1-c2}
	(|*|) (Vec3D {x=a1,y=b1,z=c1}) (Vec3D {x=a2,y=b2,z=c2}) = a1*a2 + b1*b2 + c1*c2
	(||?) (Vec3D {x=a1,y=b1,z=c1}) (Vec3D {x=a2,y=b2,z=c2}) = (a1*a2 + b1*b2 + c1*c2)^2 == (a1*a1+b1*b1+c1*c1)*(a2*a2+b2*b2+c2*c2)
	(|-?) (Vec3D {x=a1,y=b1,z=c1}) (Vec3D {x=a2,y=b2,z=c2}) = (a1*a2 + b1*b2 + c1*c2) == 0
