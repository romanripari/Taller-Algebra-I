sumatoria :: Int -> Int
sumatoria n
 | n == 1 = 1
 | otherwise  = sumatoria (n - 1) + n 

gauss n = n * (n+1) / 2 

-- Tarea:
{-
Implementar y dar el tipo de las siguientes funciones simil Ejercicio 4 Práctica 2.
-}

f1 :: Int -> Int
f1 n
 | n == 0 = 1
 | otherwise = 2 ^ n + (f1 (n - 1))

f2 ::  Int -> Float -> Float
f2 n q 
 | n == 1 = q
 | otherwise = q ^ n + (f2 (n-1) q)

f3 :: Int -> Float -> Float 
f3 n q 
 | n == 0 = 0 
 | otherwise = q ^ (2*n) + q ^ (2*n-1) + (f3 (n - 1) q)

f4 :: Int -> Float -> Float 
f4 n q 
 | n == 0 = 0 
 | otherwise = q ^ (2*n) + q ^ (2*n-1) + (f3 (n - 1) q)



