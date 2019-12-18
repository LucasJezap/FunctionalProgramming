-- product type example (one constructor)
type X = Int
type Y = Int
data CartInt2DVec = MkCartInt2DVec X Y -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> X
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Y
yCoord (MkCartInt2DVec _ y) = y


data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y


data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x


-- enum type example (special case of sum type)
data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"


{-
uwaga: ta sama nazwa* dla:
 - konstruktora typu (po lewej)
 - konstruktora danych/wartości (po prawej)

 * druga (obok omówionej poprzednio -- z prefiksem 'Mk') powszechna konwencja w Haskellu!
-}
data Cart3DVec a = Cart3DVec a a a

xCoord'' :: Cart3DVec a -> a
xCoord'' (Cart3DVec x _ _) = x

yCoord'' :: Cart3DVec a -> a
yCoord'' (Cart3DVec _ y _) = y

zCoord'' :: Cart3DVec a -> a
zCoord'' (Cart3DVec _ _ z) = z

data Card3DVec' a = Card3DVec' {x0::a, y0::a, z0::a}

polarToCartesian3D :: Floating a => a -> a -> a -> Cart3DVec a
polarToCartesian3D r phi rho = Cart3DVec (r * sin phi * cos rho) (r * sin phi * sin rho) (r * cos phi)

data Shape = Circle Float |
             Rectangle Float Float deriving Show

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b

data Tree a = EmptyT |
              Node a (Tree a) (Tree a)
              deriving Show

              
rootValue :: Tree a -> a
rootValue (EmptyT) = error "Empty Tree"
rootValue (Node x _ _) = x


data TrafficLights = Red2 | Yellow | Green

actionFor :: TrafficLights -> String
actionFor Red2 = "STOP"
actionFor Yellow = "Careful !!"
actionFor Green = "GOOOOOO"
