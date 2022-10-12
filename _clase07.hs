type Set a = [a]

vacio :: Set Int
vacio = []

agregar :: Int -> Set Int -> Set Int
agregar a c 
 | elem a c = c
 | otherwise = a : c

agregarC :: Set Int -> Set (Set Int)-> Set (Set Int)
agregarC a c 
 | elem a c = c
 | otherwise = a : c

--incluido :: Set Int -> Set Int -> Bool
--incluido (x:xs) c2 = elemento_incluido x xs c2 

incluido :: Set Int -> Set Int -> Bool
incluido [] _ = True
incluido (x:xs) c = elem x c && incluido xs c


elemento_incluido :: Int -> Set Int -> Set Int -> Bool
elemento_incluido elemento (x:xs) c2 
 | not (elem elemento c2) = False
 | xs == vacio = True 
 | otherwise = elemento_incluido x xs c2 

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

partes :: Int -> Set (Set Int)
partes n 
 | n == 0 = [vacio]
 | otherwise = (partes (n-1)) ++ (agrego_a_cada_conjunto n (partes (n-1))) 
-- | otherwise =  (agrego_a_cada_conjunto n (partes (n-1))) ++ (partes (n-1)) 

agrego_a_cada_conjunto :: Int -> Set (Set Int) -> Set (Set Int)
agrego_a_cada_conjunto n [] = []
agrego_a_cada_conjunto n (x:xss)
 | (x:xss) == [vacio] = [[n]]
 | otherwise =  agregarC  (agregar n x)  (agrego_a_cada_conjunto n xss)


