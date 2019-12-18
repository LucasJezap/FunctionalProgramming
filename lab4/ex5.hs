data MyInt = MkMyInt Int

instance Eq MyInt where
    (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
    (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2


instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromIntegral int)

instance Show MyInt where
    show (MkMyInt i) = "MkMyInt " ++ show i
	
	
data Fraction a = Fraction {num::a, denom::a} -- num - numerator, denom - denominator

instance (Show a) => Show (Fraction a) where
	show (Fraction {num = x, denom = y}) = show x ++ "/" ++ show y
	
instance (Num a, Eq a) => Eq (Fraction a) where
	(==) (Fraction {num = x1, denom = y1}) (Fraction {num = x2, denom = y2}) = (x1 * y2 == x2 * y1)
	
instance (Num a, Ord a) => Ord (Fraction a) where
	(<=) (Fraction {num = x1, denom = y1}) (Fraction {num = x2, denom = y2}) = if (signum y1 == signum y2) then (x1 * y2 <= x2 * y1) else (x1 * y2 >= x2 * y1)
	
instance (Num a) => Num (Fraction a) where
	(+) (Fraction {num = x1, denom = y1}) (Fraction {num = x2, denom = y2}) = Fraction {num = x1*y2+x2*y1, denom = y1*y2}
	(*) (Fraction {num = x1, denom = y1}) (Fraction {num = x2, denom = y2}) = Fraction {num = x1*x2, denom = y1*y2}
	negate (Fraction {num = x1, denom = y1}) = Fraction {num = (-1)*x1, denom = y1}
	abs (Fraction {num = x1, denom = y1}) = Fraction {num = abs(x1), denom = abs(y1)}
	signum (Fraction {num = x1, denom = y1}) = Fraction {num = signum (x1*y1), denom = 1}
	fromInteger int = Fraction {num = fromIntegral int, denom = 1}