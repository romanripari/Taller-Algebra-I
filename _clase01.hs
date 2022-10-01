f x y = x * x + y * y

g x y z = x + y + z * z

doble x = x * 2

suma :: Float -> Float -> Float
--suma x y = 2 ** 8000000000
suma x y = x + y

sumaTupla (x, y) = x + y

--Funciones para sacar la hipotenusa
normaVectorial x1 x2 = sqrt(x1 ** 2 + x2 ** 2 ) 
normaVectorial2 x1 x2 = (x1 ** 2 + x2 ** 2 ) ** (1/2)

funcionConstante8 = 8

func n
 | n > 0 = 1
 | n == 0 = 0
 | otherwise = -1

-- Una especie de IF ELIF ELSE
{-
signo n
 | n > 0 = 1
 | n == 0 = 0
 | otherwise = -1
-}

{--
maximo x y
 | x >= y = x
 | otherwise = y
--}

--Funciones cuando no hay otherwise: Non-exhaustive patterns
f1 n
 | n >= 3 = 5

f2 n
 | n >= 3 = 5
 | n <= 1 = 8
     
f3 n
 | n >= 3 = 5
 | n == 2 = undefined
 | otherwise = 8
 
{--
El orden de las guardas importan: se leen desde la primera.
En estas funciones se pisan las imágenes
--}
f4 n
 | n >= 3 = 5
 | n <= 9 = 7
 
f5 n
 | n <= 9 = 7
 | n >= 3 = 5

-- Casos particulares primero, casos generales después
f0 0 = 1
f0 n = 0

-- Si se pone al revés, funcione pero advierte que es redundante
signo2 0 = 0
signo2 n
 | n > 0 = 1
 | otherwise = -1

cantidadDeSoluciones b c 
 | b^2 - 4*c > 0 = 2
 | b^2 - 4*c == 0 = 1
 | otherwise = 0
 
cantidadDeSoluciones2 b c 
 | d > 0 = 2
 | d == 0 = 1
 | otherwise = 0
 where d = b^2 - 4*c 
 
funcionRara1 :: Float -> Float -> Bool -> Bool
funcionRara1 x y z = ( x >= y ) || z

--o bien:
funcionRara :: Float -> Float -> Bool -> Bool
funcionRara _ _ True = True
funcionRara x y False = x >= y

-- DOMINIO Y CODOMINIO = Signatura

maximo :: Int -> Int -> Int 
maximo x y
 | x >= y = x
 | otherwise = y


signo :: Int -> Int
signo n
 | n > 0 = 1
 | n == 0 = 0
 | otherwise = -1


esPar :: Int -> Bool
esPar n = mod n 2 == 0

esImpar :: Int -> Bool
esImpar n = not (esPar n)

-- Tarea:
absoluto n   
 | n < 0 = (-n)
 | otherwise = n

maximoabsoluto m n  
 |(absoluto m) > (absoluto n) = m
 |otherwise = n

maximo3 x y z=  maximo (maximo x y) z

algunoEs0 x y = x == 0 || y == 0

ambosSon0 x y = x == 0 && y == 0

esMultiploDe m n = (mod m n == 0) || (mod n m == 0  )

digitoUnidades n = mod n 10 

digitoDecenas n = mod (div n 10) 10
