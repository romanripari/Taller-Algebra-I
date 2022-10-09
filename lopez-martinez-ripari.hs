-- López Garro, Román
-- Martínez, Fernando
-- Ripari, Román Ariel

-- Ejercicio 1. sonCoprimos
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = mcd a b == 1

-- Auxiliares:
mcd :: Integer -> Integer -> Integer
mcd a b
 | b == 0 = a
 | a > b = mcd b (mod a b)
 | otherwise = mcd a (mod b a)
-- Devuelve el minimo comun divisor implementando el algoritmo de Euclides.

-- Ejercicio 2. es2Pseudoprimo 
es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo p = esKPseudoprimo 2 p

-- Auxiliares:
esKPseudoprimo :: Integer -> Integer -> Bool
esKPseudoprimo k n
 | mod calculo n == 0 && not (esPrimo n)  = True
 | otherwise = False
  where calculo = (k ^ (n-1)) - 1
-- Devolverá True si es divisor del "cálculo" expresado en el where y si es compuesto (no primo)

esPrimo :: Integer -> Bool
esPrimo n = n /= 1 && menorDivisor n == n

menorDivisor :: Integer -> Integer
menorDivisor dividendo = primerDivisorDesde dividendo 2

primerDivisorDesde :: Integer -> Integer -> Integer
primerDivisorDesde dividendo divisor
 | mod dividendo divisor == 0 = y
 | otherwise = primerDivisorDesde dividendo (divisor+1)

-- Ejercicio 3. cantidad3Pseudoprimos 
cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos n
 | n == 1 = 0
 | es3Pseudoprimo n = 1 + cantidad3Pseudoprimos (n-1)
 | otherwise = cantidad3Pseudoprimos (n-1)

-- Auxiliares:
es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo p = esKPseudoprimo 3 p

-- Ejercicio 4
kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo k = kesimo2y3PseudoprimoDesde k 3

-- Auxiliares:
kesimo2y3PseudoprimoDesde :: Integer -> Integer -> Integer
kesimo2y3PseudoprimoDesde k n
 | k == 0 = (n-1)
 | es2Pseudoprimo (n) && es3Pseudoprimo (n) = kesimo2y3PseudoprimoDesde (k-1) (n+1)
 | otherwise = kesimo2y3PseudoprimoDesde k (n+1)

-- Ejercicio 5
esCarmichael :: Integer -> Bool
esCarmichael n = esCarmichaelDesde n 1

-- Auxiliares:
esCarmichaelDesde :: Integer -> Integer -> Bool
esCarmichaelDesde n base
 | base = n = True
 | not (esKPseudoprimo base n) = False
 | otherwise = esCarmichaelDesde n (siguienteCoprimo n (base+1))

siguienteCoprimo :: Integer -> Integer -> Integer
siguienteCoprimo n base
 | sonCoprimos n base = base
 | otherwise = siguienteCoprimo n (base+1)
