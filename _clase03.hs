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


-- Tarea:
{-
1 Implementar la función fib : Z≥0 → Z que devuelve el i-ésimo número de Fibonacci.
2 Implementar una función parteEntera :: Float -> Integer que calcule la parte entera de un número real positivo
3 Escribir una función para determinar si un número natural es múltiplo de 3. No está permitido utilizar mod ni div
4 Implementar la función sumaImpares :: Int -> Int que dado n ∈ N sume los primeros n números impares. Ej: sumaImpares 3 1+3+5 9.
5 Escribir una función medioFact que dado n ∈ N calcula n!! = n (n − 2)(n − 4) · · · . Por
ejemplo:
medioFact 10 10 ∗ 8 ∗ 6 ∗ 4 ∗ 2 3840.
medioFact 9 9 ∗ 7 ∗ 5 ∗ 3 ∗ 1 945.
-}

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
 | otherwise = (2 * n - 1) + sumaImpares ((n-1))

medioFactorial n
 | n == 0 = 1
 | n == 1 = 1
 | n > 0 = n * medioFactorial (n-2)

{--
1 Escribir una función que determine la suma de dígitos de un número positivo. Para esta
función pueden utilizar div y mod.

2 Implementar una función que determine si todos los dígitos de un número son iguales.
--}

sumaDigitos n
 | n < 10 = n
 | otherwise = mod n 10 + sumaDigitos (div n 10)

digitosIguales n
 | n < 10 = True
 | mod n 10 == mod (div n 10) 10 = digitosIguales (div n 10) 
 | otherwise = False
