sumaDivisores :: Int -> Int
sumaDivisores x = sumaDivisoresHasta x x 


sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta x 1 = 1
sumaDivisoresHasta x y
 | mod x y == 0 = y + sumaDivisoresHasta x (y-1)
 | otherwise = sumaDivisoresHasta x (y-1)



menorDivisor :: Int -> Int
menorDivisor x = auxiliar x 2

auxiliarDivisor :: Int -> Int -> Int
auxiliarDivisor x y 
 | mod x y == 0 = y
 | otherwise = auxiliarDivisor x (y+1)
 
esPrimo :: Int -> Bool
esPrimo x
 | x == 1 = False 
 | otherwise = menorDivisor x == x

nEsimoPrimo :: Int -> Int
nEsimoPrimo n = primoOrden n 2 1
 
primoOrden :: Int -> Int -> Int -> Int
primoOrden n x m
 | n == m = x
 | esPrimo (x+1) == True =  primoOrden n (x+1) (m+1)
 | esPrimo (x+1) == False =  primoOrden n (x+1) m
 

