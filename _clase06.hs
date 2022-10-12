{-
lista de enteros que comienza en 1 y termina en -100
-}

a = [1,0..(-100)]

{-
lista creciente enrte -20 y 20, congruentes a 1 modulo 4
-}

b = [-19,-15..20]


sumatoria :: [Int] -> Int
{-
sumatoria l 
 | l == [] = 0
 | otherwise = head l + sumatoria (tail l)
-}
-- Pattern matching:
sumatoria [] = 0
sumatoria ( x : xs ) = x + sumatoria xs

longitud :: [Int] -> Int
{-
longitud l 
 | l == [] = 0
 | otherwise = 1 + longitud (tail l)
-}
--Pattern matching:
longitud [] = 0
longitud ( _ : xs ) = 1 + longitud xs

pertenece :: Int -> [Int] -> Bool
{-
pertenece n [] = False
pertenece n l
 | head l == n = True
 | otherwise = pertenece n (tail l)
-}
--Pattern matching:
pertenece n [] = False
pertenece n ( x : xs) = x == n || pertenece n xs


{-
▶ productoria :: [Int] -> Int que devuelve la productoria de los elementos de una lista.
▶ sumarN :: Int -> [Int] -> [Int] que dado un n ́umero N y una lista xs, suma N a cada elemento de xs.
▶ sumarElPrimero :: [Int] -> [Int] que dada una lista no vac ́ıa xs, suma el primer elemento a cada elemento de xs. Ejemplo: sumarElPrimero [1,2,3] ⇝ [2,3,4]
▶ sumarElUltimo :: [Int] -> [Int] que dada una lista no vac ́ıa xs, suma el  ́ultimo elemento a cada elemento de xs. Ejemplo: sumarElUltimo [1,2,3] ⇝ [4,5,6]
▶ pares :: [Int] -> [Int] que devuelve una lista con los elementos pares de una lista dada. Ejemplo: pares [1,2,3,5,8] ⇝ [2,8]
▶ multiplosDeN :: Int -> [Int] -> [Int] que dado un n ́umero N y una lista xs, devuelve una lista con los elementos m ́ultiplos N de xs.
▶ reverso :: [Int] -> [Int] que dada una lista invierte su orden.
▶ maximo :: [Int] -> Int que calcula el m ́aximo elemento de una lista no vac ́ıa.
▶ ordenar :: [Int] -> [Int] que ordena los elementos de una lista de forma creciente.
▶ quitar :: Int -> [Int] -> [Int] que elimina la primera aparici ́on del elemento en la lista (de haberla).
▶ hayRepetidos :: [Int] -> Bool que indica si una lista tiene elementos repetidos.
▶ eliminarRepetidos :: [Int] -> [Int] que deja en la lista una  ́unica aparici ́on de cada elemento, eliminando las repeticiones adicionales.
-}

productoria :: [Int] -> Int
productoria l
 | l == [] = 1
 | otherwise = head l * productoria (tail l)

productoria_PM :: [Int] -> Int
productoria_PM [] = 1
productoria_PM ( x : xs ) = x * productoria_PM xs

sumarN :: Int -> [Int] -> [Int]
sumarN n l
 | l == [] = []
 | otherwise = n + (head l) : sumarN n (tail l)

sumarN_PM :: Int -> [Int] -> [Int]
sumarN_PM n [] = []
sumarN_PM n (x:xs) = n + x : sumarN_PM n xs

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero l = sumarN (head l) l 

sumarElPrimero_PM :: [Int] -> [Int]
sumarElPrimero_PM [] = []
sumarElPrimero_PM ( x : xs ) = sumarN_PM x ( x : xs )

-- Auxiliar
el_ultimo :: [Int] -> Int
el_ultimo [] = 0
el_ultimo (x : xs)
 | xs == [] = x
 | otherwise = el_ultimo xs
 
sumarElUltimo :: [Int] -> [Int]
sumarElUltimo l = sumarN (el_ultimo l) l 

sumarElUltimo_PM :: [Int] -> [Int]
sumarElUltimo_PM [] = []
sumarElUltimo_PM ( x : xs ) = sumarN_PM (el_ultimo ( x : xs )) ( x : xs )

pares :: [Int] -> [Int]
pares l
 | l == [] = []
 | mod (head l) 2 == 0 = (head l) : pares (tail l)
 | otherwise = pares (tail l)

pares_PM :: [Int] -> [Int]
pares_PM [] = []
pares_PM ( x : xs )
 | mod x 2 == 0 = x : pares_PM xs
 | otherwise = pares_PM xs

multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN n l
 | l == [] = []
 | mod (head l) n == 0 = (head l) : multiplosDeN n (tail l)
 | otherwise = multiplosDeN n (tail l)

multiplosDeN_PM :: Int -> [Int] -> [Int]
multiplosDeN_PM n [] = []
multiplosDeN_PM n ( x: xs )
 | mod x n == 0 = x : multiplosDeN_PM n xs
 | otherwise = multiplosDeN_PM n xs

-- Auxiliar
quitarUltimo :: [Int] -> [Int]
quitarUltimo [] = []
quitarUltimo ( x : xs ) 
 | xs == [] = []
 | otherwise = x : quitarUltimo xs

reverso :: [Int] -> [Int]
reverso l
 | l == [] = []
 | otherwise = el_ultimo l : reverso (quitarUltimo l)

reverso_PM :: [Int] -> [Int]
reverso_PM [] = []
reverso_PM ( x: xs ) = el_ultimo xs : reverso_PM (quitarUltimo (x:xs))

maximo :: [Int] -> Int
maximo l 
 | l == [] = 0
 | otherwise = maximoQue (head l) (tail l)
 
maximo_PM :: [Int] -> Int
maximo_PM [] = 0
maximo_PM ( x : xs ) = maximoQue x xs

-- Auxiliar
maximoQue :: Int -> [Int] -> Int
maximoQue n [] = n
maximoQue n ( x : xs) 
 | n >= x = maximoQue n xs
 | otherwise = maximoQue x xs

ordenar :: [Int] -> [Int] 
ordenar l
 | l == [] = []
 | tail l == [] = primero : []
 | primero < primero_del_resto = primero : ordenar (primero_del_resto : tail resto_ordenado)
 | otherwise = primero_del_resto : ordenar (primero : tail resto_ordenado)

 where primero_del_resto = head resto_ordenado
       primero = head l
       resto_ordenado = ordenar (tail l)



ordenar_PM :: [Int] -> [Int] 
ordenar_PM [] = []
ordenar_PM ( x : xs ) 
 | xs == [] = x : []
 | x > (head (ordenar_PM xs)) = (head (ordenar_PM xs)) : ordenar_PM (x : tail (ordenar_PM xs) )
 | otherwise = x : ordenar_PM ((head (ordenar_PM xs)) : tail (ordenar_PM xs) )

quitar :: Int -> [Int] -> [Int]
quitar n l
 | l == [] = []
 | head l == n = (tail l)
 | otherwise = (head l) : quitar n (tail l)

quitar_PM :: Int -> [Int] -> [Int]
quitar_PM n [] = []
quitar_PM n ( x : xs) 
 | x == n = xs
 | otherwise = x: quitar_PM n xs

hayRepetidos :: [Int] -> Bool
hayRepetidos l
 | l == [] = False
 | tail l == [] = False
 | head (ordenar_PM l) == head (tail (ordenar_PM l)) = True
 | otherwise = hayRepetidos (tail (ordenar_PM l))

hayRepetidos_PM :: [Int] -> Bool
hayRepetidos_PM [] = False
hayRepetidos_PM ( x : xs )
 | xs == [] = False
 | head (ordenar_PM (x:xs)) == head (tail (ordenar_PM (x:xs))) = True
 | otherwise = hayRepetidos_PM (tail (ordenar_PM (x:xs)))

eliminarRepetidos :: [Int] -> [Int]
eliminarRepetidos l
 | l == [] = []
 | tail l == [] = l
 | head (ordenar_PM l) == head (tail (ordenar_PM l)) = tail (ordenar_PM l)
 | otherwise = head (ordenar_PM l) : eliminarRepetidos (tail (ordenar_PM l))














