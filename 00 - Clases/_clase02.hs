beta :: Bool -> Int
beta False = 0
beta _ = 1

identidad :: a -> a
identidad x = x

primero :: a -> b -> a
primero x y = x

segundo :: a -> b -> b
segundo x y = y

constante5 :: a -> b -> c -> Int  
constante5 x y z = 5

mismoTipo :: t -> t -> Bool
mismoTipo x y = True

triple ::Num a => a -> a
triple x = x * 3

maximo :: Ord a => a -> a -> a 
maximo x y
 | x >= y = x
 | otherwise  = y

-- distintos :: a -> a -> Bool 
distintos x y = x /= y

cantidadDeSoluciones :: (Num t, Ord t) => t -> t -> Int
cantidadDeSoluciones b c 
 | d > 0 = 2
 | d == 0 = 1
 | otherwise = 0
 where d = b^2 - 4*c 
 
pepe :: (Floating t , Eq t , Num u , Eq u ) => t -> t -> u -> Bool
pepe x y z = sqrt ( x + y ) == x && 3* z == 0


f1 :: (Floating n, Ord n) => n -> n -> n -> Bool
f1 x y z = x ** y + z <= x + y**z

f2 :: Floating a => a -> a -> a
f2 x y = (sqrt x) / (sqrt y)

f3 :: (Integral a, Floating a) => a -> a -> a
f3 x y =  div (sqrt x) (sqrt y)

--f4 :: (Eq a, Floating a, Num a) => a -> a -> a -> a
f4 x y z 
 | x == y = z 
 | x ** y == y = x
 | otherwise = y
 
--f5 :: (Eq a, Floating a) => a -> a -> a -> a
f5 x y z 
 | x == y = z 
 | x ** y == y = z
 | otherwise = z


suma :: ( Float , Float ) -> ( Float , Float ) -> ( Float , Float )
suma v w = (( fst v ) + ( fst w ) , ( snd v ) + ( snd w ) )

esOrigen :: ( Float , Float ) -> Bool
esOrigen (a, b)
 | (a, b) == (0 , 0) = True
 | otherwise = False

angulo0 :: ( Float , Float ) -> Bool
angulo0 (a, b)
 | (a, b) == (a , 0) = True
 | (a, b) == (0 , b) = True
 | otherwise = False


