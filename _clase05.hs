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
--nEsimoPrimo n = primoascendiendo n 2 1
 
primoascendiendo :: Int -> Int -> Int -> Int
primoascendiendo n x m
 | n == m = x
 | esPrimo (x+1) == True =  primoascendiendo n (x+1) (m+1)
 | esPrimo (x+1) == False =  primoascendiendo n (x+1) m
 
primoDescendiendo :: Int -> Int -> Int
primoDescendiendo n x
 | n == 1 = x
 | esPrimo (x+1) == False = primoDescendiendo n (x+1)
 | esPrimo (x+1) == True = primoDescendiendo (n-1) (x+1)
