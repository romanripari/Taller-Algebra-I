-- López Garro, Román
-- Martínez, Fernando
-- Ripari, Román Ariel

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
-- Llamamos a la función mcd a b. Si el MCD entre ambos es 1, significa que son coprimos

-- Auxiliares:
mcd :: Integer -> Integer -> Integer
mcd a b
 | b == 0 = a
 | a > b = mcd b (mod a b)
 | otherwise = mcd a (mod b a)
-- Utilizamos el teorema de Euclides: el MCD entre dos números es el MCD entre el menor y el resto entre ambos.
-- El caso base, cero, indica que ya llegamos a la reducción mínima y ahí devolvemos el primer valor. 
-- Ejemplos:
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
-- Utilizamos esKPseudoprimo, ya que será necesaria para los próximos ejercicios, con k = 2 

-- Auxiliares:
esKPseudoprimo :: Integer -> Integer -> Bool
esKPseudoprimo k n
 | mod calculo n == 0 && not (esPrimo n)  = True
 | otherwise = False
  where calculo = (k ^ (n-1)) - 1
-- Devolverá True si es divisor del "cálculo" expresado en el where y si es compuesto (no primo)

esPrimo :: Integer -> Bool
esPrimo x = x /= 1 && menorDivisor x == x
-- Devolverá True si no es 1, y si su menor Divisor es el mismo número

menorDivisor :: Integer -> Integer
menorDivisor dividendo = auxiliarDivisorDesde dividendo 2
-- Función intermedia: llama al auxiliarDivisorDesde con el dividendo y 2 para encontrar el divisor

auxiliarDivisorDesde :: Integer -> Integer -> Integer
auxiliarDivisorDesde x y
 | mod x y == 0 = y
 | otherwise = auxiliarDivisorDesde x (y+1)
-- La recursión es llamada con 2 y verifica si algun numero (ascendente en +1) es divisor del dividendo.
-- Devuelve el primero que encuentra

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
-- Verifica si n es3Pseudoprimo, si es así suma 1 y verifica el n anterior. 
-- Si no es3Pseudoprimo, no suma nada y verifica el n anterior

-- Auxiliares:
es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo p = esKPseudoprimo 3 p
-- Utilizamos esKPseudoprimo con k = 3 

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
kesimo2y3Pseudoprimo k = kesimo2y3PseudoprimoDesde k 3
-- Llamamos a la función auxiliar kesimo2y3PseudoprimoDesde para contar, desde 3, el késimo 2-3-pseudoPrimo 

-- Auxiliares:
kesimo2y3PseudoprimoDesde :: Integer -> Integer -> Integer
kesimo2y3PseudoprimoDesde k n
 | k == 0 = (n-1)
 | es2Pseudoprimo (n) && es3Pseudoprimo (n) = kesimo2y3PseudoprimoDesde (k-1) (n+1)
 | otherwise = kesimo2y3PseudoprimoDesde k (n+1)
-- Empezando en n = 3, ascendemos uno a uno. Cada vez que algún n es a la vez 2-3pseudoPrimo, restamos k-1.
-- Si k = 0, significa que en la recursión anterior encontramos el késimo 2-3pseudoPrimo, y lo devolvemos

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
esCarmichael n = esCarmichaelDesde n 1
-- Llamamos a la función esCarmichaelDesde, comenzando con base = 1 para corroborar que cumpla la condición 
-- Condición 

-- Auxiliares:
esCarmichaelDesde :: Integer -> Integer -> Bool
esCarmichaelDesde n base
 | base >= n = True
 | not (esKPseudoprimo base n) = False
 | otherwise = esCarmichaelDesde n (siguienteCoprimo n (base+1) )
-- En cada vuelta de la recursión la base es el siguiente coprimo de n, con la función auxiliar siguienteCoprimo
-- En cada vuelta verificamos si el n es esKPseudoprimo, con k = base
-- Si en alguna vuelta de la recursión n no es eskPseudoprimo (k = base), entonces no esCarmichael y devolvemos False
-- Si la base aumenta hasta llegar a n (o más), significa que se cumple la condición entre 1 y (n-1) y esCarmichael. Retornamos True

siguienteCoprimo :: Integer -> Integer -> Integer
siguienteCoprimo n base
 | sonCoprimos n base = base
 | otherwise = siguienteCoprimo n (base+1)
-- Función auxiliar para encontrar el siguiente coprimo de n, desde un número base.
-- Si sonCoprimos devuelve la base. Si no, prueba con el siguiente. El máximo que podría devolver es el mismo n.
