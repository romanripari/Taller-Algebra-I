division :: Int -> Int -> ( Int , Int )
division a d
 | a < 0 = (-q-1, d-r) --Si el primero es negativo
 | d < 0 = (-q', r')   --Si el segundo es negativo
 | a < d = (0 , a )
 | otherwise = ( fst ( division (a - d ) d ) + 1, snd ( division (a - d ) d ))
 where (q,r) = division (-a) d 
       (q',r') = division a (-d) 

mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b  = mcd b (snd (division a b))


todosLosDivisores :: Int -> [Int]
todosLosDivisores x = todosLosDivisoresAux x x
--todosLosDivisores x = 1 : auxiliarDivisor x 2

todosLosDivisoresAux :: Int -> Int -> [Int]
todosLosDivisoresAux x x'
 | x == 0 = []
 | x == 1 = [1] 
 | mod x' x == 0 = x : (todosLosDivisoresAux (x-1) x')
 | otherwise = (todosLosDivisoresAux (x-1) x')

auxiliarDivisor :: Int -> Int -> [Int]
auxiliarDivisor x y 
 | x == y = [x]
 | mod x y == 0 = y : auxiliarDivisor x (y+1)
 | otherwise = auxiliarDivisor x (y+1)

mayorDivisorComun :: Int -> Int -> Int
mayorDivisorComun a b = elemento_comun ((todosLosDivisores a)) ((todosLosDivisores b)) 

elemento_comun :: [Int] -> [Int] -> Int
elemento_comun as bs 
 | elem (head as) bs = head as
 | otherwise =  elemento_comun (tail as) bs

{-
quitarUltimo :: [Int] -> [Int]
quitarUltimo [] = []
quitarUltimo ( x : xs ) 
 | xs == [] = []
 | otherwise = x : quitarUltimo xs

el_ultimo :: [Int] -> Int
el_ultimo [] = 0
el_ultimo (x : xs)
 | xs == [] = x
 | otherwise = el_ultimo xs
-}


emcd :: Int -> Int -> (Int, Int, Int)
emcd a b = (mcd a b, obtengoS a b, obtengoT a b) 



obtengoS :: Int -> Int -> Int
obtengoS a b = 0

obtengoT :: Int -> Int -> Int
obtengoT a b = 0

{-
s = t'
t = (s' - t' * g )
-}





















