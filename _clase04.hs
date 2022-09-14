sumatoria :: Int -> Int
sumatoria n
 | n == 1 = 1
 | otherwise  = sumatoria (n - 1) + n 

gauss n = n * (n+1) / 2 

-- Tarea:
{-
Implementar y dar el tipo de las siguientes funciones simil Ejercicio 4 Práctica 2.
-}

f1 :: Int -> Int
f1 n
 | n == 0 = 1
 | otherwise = 2 ^ n + (f1 (n - 1))

f2 ::  Int -> Float -> Float
f2 n q 
 | n == 1 = q
 | otherwise = q ^ n + (f2 (n-1) q)


{-
Otra opción:
f3 :: Int -> Float -> Float 
f3 n q 
 | n == 1 = q ^ 2 + q   
 | otherwise = q ^ (2*n) + q ^ (2*n-1) + (f3 (n - 1) q)
-}

f3 :: Int -> Float -> Float 
f3 n q = f2 (2*n) q

f4 :: Int -> Float -> Float 
f4 n q = f4_aux (2 * n) q n

f4_aux :: Int -> Float -> Int -> Float 
f4_aux n q i 
 | n == i = q ^ n
 | otherwise = q ^ n + (f4_aux (n-1) q i)

{-
Otra opción:
-}
f4_b :: Int -> Float -> Float 
f4_b n q = f3 n q - f2 (n-1) q

{-
Implementar una función eAprox :: Integer -> Float que aproxime el valor del número
e a partir de la siguiente sumatoria:
	n
e(n) = ∑ 1/i!
	i=0

I Definir la constante e :: Float como la aproximaci ́on de e a partir de los primeros 10
t ́erminos de la serie anterior.
-}

factorial :: Int -> Int
factorial n
 | n == 1 = 1
 | otherwise = n * factorial(n-1) 

eAprox :: Int -> Float
eAprox n 
 | n == 0 = 1 
 | otherwise = 1 / fromIntegral(factorial (n) ) + eAprox ( n-1 ) 

e :: Float
e = eAprox 10

{-
Ejercicios (sumatorias dobles)
1) Implementar la siguiente función:
	   n   m 
f (n, m) = ∑   ∑ ij
	   i=1 j=1

2) Implementar una función sumaPotencias q n m que sume todas las potencias de la forma

q^(a+b) con 1 ≤ a ≤ n y 1 ≤ b ≤ m.

3) Implementar una función sumaRacionales n m que sume todos los n ́umeros racionales de la forma p/q con 1 ≤ p ≤ n y 1 ≤ q ≤ m.
-}



-- 1)
sumaInterna :: Int -> Int -> Int
sumaInterna m i 
 | m == 0 = 1
 | otherwise = m ^ i + sumaInterna (m-1) i 

sumatoriaDoble :: Int -> Int -> Int
sumatoriaDoble n m 
 | n == 0 = 1
 | otherwise = sumaInterna m n + sumatoriaDoble (n-1) m


-- 2)
sumaPotenciaInterna :: Float -> Int -> Int -> Float
sumaPotenciaInterna q a b
 | b == 0 = 0
 | otherwise = q ^ (a+b) + sumaPotenciaInterna q a (b-1) 

sumaPotencias :: Float -> Int -> Int -> Float
sumaPotencias q n m 
 | n == 0 = 0
 | otherwise = sumaPotenciaInterna q n m + sumaPotencias q (n-1) m


-- 3)
sumaRacionalesInterna :: Int -> Int -> Float
sumaRacionalesInterna p q 
 | q == 1 = fromIntegral(p) / fromIntegral(q)
 | otherwise = fromIntegral(p) / fromIntegral(q) + sumaRacionalesInterna p (q-1) 

sumaRacionales :: Int -> Int -> Float
sumaRacionales n m 
 | n == 0 = 0
 | otherwise = sumaRacionalesInterna n m + sumaRacionales (n-1) m



{--
4)
Implementar la siguiente función:
g1(i, n) ...

5) Implementar la siguiente función:
g2(n) = ...

6) Implementar la siguiente función:
g3(n) = ...

7) Implementar una función que dado un n, sume todos los números naturales menores o
iguales que n que tengan todos los dígitos iguales.
--}


--4)
g1 i n 
 | n == i = i ^ n
 | otherwise = i ^ n + g1 i (n-1)

--5)
g2_Interna :: Int -> Int 
g2_Interna n
 | n == 0 = 1
 | otherwise = n ^ n + g2_Interna (n-1) 

g2 :: Int -> Int
g2 n 
 | n == 0 = 1
 | otherwise = g2_Interna n + g2 (n-1)

--6)



--7)
