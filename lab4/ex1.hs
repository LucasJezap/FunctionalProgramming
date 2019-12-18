polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r,phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r * cos phi, r * sin phi) 	

newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCoord'' a = MkPolarCoord'' (a,a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)




cylindricToCartesian :: Floating a => (a,a,a) -> (a,a,a)
cylindricToCartesian (p,phi,z) = (p * cos phi, p * sin phi, z)

type CartesianCoord2' a = (a,a,a)
type CylindricCoord' a = (a,a,a)

cylindricToCartesian' :: Floating a => CylindricCoord' a -> CartesianCoord2' a
cylindricToCartesian' (p,phi,z) = (p * cos phi, p * sin phi, z)	

newtype CartesianCoord2'' a = MkCartesianCoord2'' (a,a,a)
newtype CylindricCoord'' a = MkCylindricCoord'' (a,a,a)

cylindricToCartesian'' :: Floating a => CylindricCoord'' a -> CartesianCoord2'' a
cylindricToCartesian'' (MkCylindricCoord'' (p,phi,z)) = MkCartesianCoord2'' (p * cos phi, p * sin phi, z)

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String


personInfoToString :: (String,String,String) -> String
personInfoToString (nm,snm,addr) =
 "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

