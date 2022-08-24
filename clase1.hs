f x y = x * x + y * y

g x y z = x + y + z * z

doble x = x * 2

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
signo n
 | n > 0 = 1
 | n == 0 = 0
 | otherwise = -1

maximo x y
 | x >= y = x
 | otherwise = y

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
 
 


