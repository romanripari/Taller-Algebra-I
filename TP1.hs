-- Apellido Nombre #1
-- Apellido Nombre #2
-- Apellido Nombre #3

{- Ejercicio 1

Escribir la función:
sonCoprimos :: Integer -> Integer -> Bool
que dados dos números naturales decide si son coprimos. Por ejemplo:
sonCoprimos 12 9
False
sonCoprimos 1007 1474
True
sonCoprimos 1 9
True
-}

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = mcd a b == 1

-- Auxiliares:
mcd :: Integer -> Integer -> Integer
mcd a b
 | b == 0 = a
 | a > b = mcd b (mod a b)
 | otherwise = mcd a (mod b a)

{-
(4 : 12)
mcd 4 12
(otherwise = mcd 4 (mod 12 4))
mcd 4 0
(b == 0 = a )
4

(7 : 10)
mcd 7 10
(otherwise = mcd 7 (mod 10 7))
mcd 7 3 
(a > b = mcd 3 (mod 7 3))
mcd 3 1
(a > b = mcd 1 (mod 3 1))
mcd 1 0
(b == 0 = a)
1
-}

{- Ejercicio 2

Escribir la función:
es2Pseudoprimo :: Integer -> Bool
que dado un número natural decide si es 2-pseudoprimo. Por ejemplo:
es2Pseudoprimo 561
True
es2Pseudoprimo 1387
True
es2Pseudoprimo 1728
False
-}

es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo p = esKPseudoprimo 2 p

-- Auxiliares:
esKPseudoprimo :: Integer -> Integer -> Bool
esKPseudoprimo k p
 | mod calculo p == 0 && not (esPrimo p)  = True
 | otherwise = False
  where calculo = (k ^ (p-1)) - 1

esPrimo :: Integer -> Bool
esPrimo x = x /= 1 && menorDivisor x == x


menorDivisor :: Integer -> Integer
menorDivisor dividendo = auxiliarDivisorDesde dividendo 2

auxiliarDivisorDesde :: Integer -> Integer -> Integer
auxiliarDivisorDesde x y
 | mod x y == 0 = y
 | otherwise = auxiliarDivisorDesde x (y+1)

{- Ejercicio 3

Escribir la función:
cantidad3Pseudoprimos :: Integer -> Integer
que dado un número natural m calcula la cantidad de 3-pseudoprimos que hay entre 1 y m inclusive. Por ejemplo:
cantidad3Pseudoprimos 100
1
cantidad3Pseudoprimos 671
4
cantidad3Pseudoprimos 702
4
-}

cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos n
 | n == 1 = 0
 | es3Pseudoprimo n = 1 + cantidad3Pseudoprimos (n-1)
 | otherwise = cantidad3Pseudoprimos (n-1)

-- Auxiliares:
es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo p = esKPseudoprimo 3 p

{- Ejercicio 4

Escribir la función:
kesimo2y3Pseudoprimo :: Integer -> Integer
que dado un número natural k calcula el k-ésimo número que es simuláneamente 2-pseudoprimo y 3-pseudoprimo. Por ejemplo:
kesimo2y3Pseudoprimo 1
1105
kesimo2y3Pseudoprimo 4
2701
kesimo2y3Pseudoprimo 6
6601
-}

kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo k = kesimo2y3PseudoprimoHasta k 3

-- Auxiliares:
kesimo2y3PseudoprimoHasta :: Integer -> Integer -> Integer
kesimo2y3PseudoprimoHasta k x
 | k == 0 = x
 | es2Pseudoprimo (x+1) && es3Pseudoprimo (x+1) = kesimo2y3PseudoprimoHasta (k-1) (x+1)
 | otherwise = kesimo2y3PseudoprimoHasta k (x+1)

{- Ejercicio 5

Escribir la función:
esCarmichael :: Integer -> Bool
que dado un número natural decide si es un número de Carmichael Por ejemplo:
esCarmichael 2465
True
esCarmichael 2821
True
esCarmichael 1541
False
-}

esCarmichael :: Integer -> Bool
esCarmichael n = esCarmichaelHasta n 1

-- Auxiliares:
esCarmichaelHasta :: Integer -> Integer -> Bool
esCarmichaelHasta n a
 | a >= n = True
 | not (esKPseudoprimo a n) = False
 | otherwise = esCarmichaelHasta n (siguienteCoprimo n (a+1) )

siguienteCoprimo :: Integer -> Integer -> Integer
siguienteCoprimo n a
 | sonCoprimos n a = a
 | otherwise = siguienteCoprimo n (a+1)