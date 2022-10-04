-- Apellido Nombre #1
-- Apellido Nombre #2
-- Apellido Nombre #3

{-


-- EJE3RCICO 2: es2Pseudoprimo
es2Pseudoprimo :: Integer -> Bool


-- EJERCICIO 3: cantidad3Pseudoprimos
cantidad3Pseudoprimos :: Integer -> Integer


-- EJERCICIO 4: kesimo2y3Pseudoprimo
kesimo2y3Pseudoprimo :: Integer -> Integer


-- EJERCICIO 5: esCarmichael
esCarmichael :: Integer -> Bool

-}

mcdRecursivo :: Integer -> Integer -> Integer
mcdRecursivo a b 
 | b == 0 = a 
 | a > b = mcdRecursivo b (mod a b)
 | otherwise = mcdRecursivo a (mod b a)

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = mcdRecursivo a b == 1  

es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo p 
 | mod cuenta p == 0 = True
 | otherwise = False 
  where cuenta = 2 ^ (p-1) - 1 

es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo p 
 | mod cuenta3 p == 0 = True
 | otherwise = False 
  where cuenta3 = (3 ^ (p-1)) - 1 

cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos n 
 | n == 0 = 0
 | es3Pseudoprimo n = 1 + cantidad3Pseudoprimos (n-1)
 | otherwise = cantidad3Pseudoprimos (n-1)

kesimo2y3Pseudoprimo :: Integer -> Integer

{-

esPrimo :: Integer -> Bool
esPrimo 

-}