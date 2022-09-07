factorial :: Int -> Int

{-
factorial x 
 | x == 1 = 1
 | otherwise = x * factorial y
 where y = x-1
-}
 

factorial n
 | n == 0 = 1
 | n > 0 = n * factorial (n-1)

{-
esPar :: Int -> Bool
esPar n
 | n == 0 = True
 | n < 0 = False
 | otherwise = esPar (n-2)
-}

esPar :: Int -> Bool
esPar n
 | n == 0 = True
 | otherwise = not (esPar (n-1))


fib :: Int -> Int
fib n
 | n == 0 = 0
 | n == 1 = 1
 | otherwise = fib(n-1) + fib (n-2)
 
parteEntera :: Float -> Integer
parteEntera x 
 | x < 1 = 0
 | otherwise = 1 + parteEntera (x-1)


multiplo3 :: Int -> Bool
multiplo3 x 
 | x == 0 = True
 | x == 1 || x == 2 = False
 | otherwise = multiplo3 (x-3)

sumaImpares :: Int -> Int
sumaImpares n
 | n == 1 = 1
 | otherwise = 2 + sumaImpares (n - 1)

medioFactorial n
 | n == 0 = 1
 | n == 1 = 1
 | n > 0 = n * medioFactorial (n-2)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
