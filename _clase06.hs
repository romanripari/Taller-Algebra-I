{-
lista de enteros que comienza en 1 y termina en -100
-}

a = [1,0..(-100)]

{-
lista creciente enrte -20 y 20, congruentes a 1 modulo 4
-}

b = [-19,-15..20]


sumatoria :: [Int] -> Int
sumatoria l 
 | l == [] = 0
 | otherwise = head l + sumatoria (tail l)

-- :: [Int] -> Int
longitud :: [Int] -> Int
longitud l 
 | l == [] = 0
 | otherwise = 1 + longitud (tail l)

pertenece :: Int -> [Int] -> Bool
pertenece n l
 | l == [] = False
 | head l == n = True
 | otherwise = pertenece n (tail l)
