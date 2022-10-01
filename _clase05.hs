sumaDivisores :: Int -> Int
sumaDivisores x = sumaDivisoresHasta x x 


sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta x 1 = 1
sumaDivisoresHasta x y
 | mod x y == 0 = y + sumaDivisoresHasta x (y-1)
 | otherwise = sumaDivisoresHasta x (y-1)



menorDivisor :: Int -> Int
menorDivisor x = auxiliarDivisor x 2

auxiliarDivisor :: Int -> Int -> Int
auxiliarDivisor x y 
 | mod x y == 0 = y
 | otherwise = auxiliarDivisor x (y+1)
 
esPrimo :: Int -> Bool
esPrimo x 
  | x == 1 = False 
  | otherwise = menorDivisor x == x

nEsimoPrimo :: Int -> Int
nEsimoPrimo n = primoDescendiendo n 2
{-
nEsimoPrimo n = primoAscendiendo n 2 1
 
primoAscendiendo :: Int -> Int -> Int -> Int
primoAscendiendo n x m
 | n == m = x
 | esPrimo (x+1) == True =  primoAscendiendo n (x+1) (m+1)
 | esPrimo (x+1) == False =  primoAscendiendo n (x+1) m
-}
 
primoDescendiendo :: Int -> Int -> Int
primoDescendiendo n x
 | n == 1 = x
 | esPrimo (x+1) = primoDescendiendo  (n-1) (x+1)
 | otherwise = primoDescendiendo  n (x+1)




menorFactDesde :: Int -> Int
menorFactDesde x 
 | esFact(x) = x
 | otherwise = menorFactDesde (x+1)


{-
Otra forma:
menorFactDesde :: Int -> Int
menorFactDesde m = menorFactDesdeDesde m 1

menorFactDesdeDesde Int -> Int -> Int
menorFactDesdeDesde m n 
 | factorial n > m = factorial n
 | otherwise = menorFactDesdeDesde m (n+1)

mayorFactHasta :: Int -> Int
mayorFactHasta m = mayorFactHastaHasta m 1

mayorFactHastaHasta Int -> Int -> Int
mayorFactHastaHasta m n 
 | factorial n > m = factorial (n-1)
 | otherwise = mayorFactHastaHasta m (n+1)

esFact :: Int -> Bool
esFact n = menorFactDesdeDesde n == factorial n

-}
mayorFactHasta :: Int -> Int
mayorFactHasta x 
 | esFact(x) = x
 | otherwise = mayorFactHasta (x-1)

esFact :: Int -> Bool
esFact x = esFactDesde x 1

esFactDesde :: Int -> Int -> Bool
esFactDesde x y 
 | y > x = False
 | (factorial y) == x = True
 | otherwise = esFactDesde x (y+1)

factorial :: Int -> Int
factorial n
 | n == 0 = 1
 | otherwise = n * factorial (n-1)




esFibonacci :: Int -> Bool
esFibonacci x = esFiboDesde x 1

esFiboDesde :: Int -> Int -> Bool
esFiboDesde x y 
 | (fib y) > x = False
 | (fib y) == x = True
 | otherwise = esFiboDesde x (y+1)

fib :: Int -> Int
fib n
 | n == 0 = 0
 | n == 1 = 1
 | otherwise = fib(n-1) + fib (n-2)


esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = esSumaPrimosDesde n 1

esSumaPrimosDesde :: Int -> Int -> Bool
esSumaPrimosDesde n m 
 | sumaPrimos 1 m == n = True
 | sumaPrimos 1 m > n = False
 | otherwise = esSumaPrimosDesde n (m+1)

sumaPrimos :: Int -> Int -> Int
sumaPrimos n m 
 | n > m = 0
 | otherwise = nEsimoPrimo n + sumaPrimos (n+1) m



{-
Implementar tomaValorMax :: Int -> Int -> Int que dado un n ́umero entero n1 ≥ 1 y
un n2 ≥ n1 devuelve alg ́un m entre n1 y n2 tal que
sumaDivisores(m) = max{sumaDivisores(i) | n1 ≤ i ≤ n2}
12 Implementar tomaValorMin :: Int -> Int -> Int que dado un n ́umero entero n1 ≥ 1 y
un n2 ≥ n1 devuelve alg ́un m entre n1 y n2 tal que
sumaDivisores(m) = min{sumaDivisores(i) | n1 ≤ i ≤ n2}

-}

























