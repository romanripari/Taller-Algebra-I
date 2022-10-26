type Set a = [a]

------------------------------------------------------------------
------------------------------------------------------------------

combinatorio :: Int -> Int -> Int
combinatorio n 0 = 1
combinatorio n m 
 | n == m = 1
 | otherwise =  combinatorio (n-1) m + combinatorio (n-1)  (m-1)

------------------------------------------------------------------
------------------------------------------------------------------
variaciones :: Set Int -> Int -> Set [Int]
variaciones c l 
 | l == 0 = [[]] 
 | otherwise = agregarTodosATodos c (variaciones c (l-1))

agregarTodosATodos :: Set Int -> Set [Int] -> Set [Int]
agregarTodosATodos [] objetivo = [] 
agregarTodosATodos c objetivo = agregarUnoATodos (head c) objetivo ++ (agregarTodosATodos (tail c) objetivo)

agregarUnoATodos :: Int -> Set [Int] -> Set [Int]
agregarUnoATodos n [] = []
agregarUnoATodos n (xs:xss) =  (n : xs) : (agregarUnoATodos n xss)
------------------------------------------------------------------
------------------------------------------------------------------

insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn l n i 
 | length l < i = l ++ [n] 
 | i <= 1 = n : l 
 | otherwise = (head l) : insertarEn (tail l) n (i-1) 

------------------------------------------------------------------
------------------------------------------------------------------
permutaciones :: Int -> Set [Int]
permutaciones 0 = [[]]
permutaciones n = recorroOrdenes n n (permutaciones (n-1)) 

-- Otra opción para Permutaciones:
permutaciones2 :: Int -> Set [Int]
permutaciones2 0 = [[]]
permutaciones2 n = recorroOrdenes n n (permutaciones2 (n-1)) 

recorroOrdenes :: Int -> Int -> Set [Int] -> Set [Int]
recorroOrdenes n i ls
 | i == 0 = []
 | otherwise = insertador n i ls ++ recorroOrdenes n (i-1) ls

insertador :: Int -> Int -> Set [Int] -> Set [Int]
insertador n i ls 
 | ls == [] = []
 | elem n (head ls) = insertador n i (tail ls)
 | otherwise = insertarEn (head ls) n i : insertador n i (tail ls)

{-
Tengo un n
Con ese n, voy restando un i (orden) hasta llegar a 1. 
Con cada n i (orden), recorremos cada elemento de la lista de listas y lo inserto
-}


{-------------------------------------------------------------------
Implementar funciones que devuelvan
1 Todas las formas de ubicar n bolitas numeradas en k cajas.
2 Implementar una función
subconjuntos :: Int -> Int -> Set (Set Int) que dados k y n enteros, genera todos
los subconjuntos de k elementos del conjunto {1, 2, 3, . . . , n}.
Ejemplo> subjconjuntos 2 3
[[1, 2], [2, 3], [1, 3]]
3 Todas las listas ordenadas de k n ́umeros distintos tomados del conjunto {1, . . . , n}.
4 Todas las sucesiones de 0 y 1 de longitud 6 en las que hay tres 1’s y tres 0’s.
5 Todas las sucesiones de 0 y 1 de longitud 5 en las que hay mas 1’s que 0’s.
------------------------------------------------------------------
-}





