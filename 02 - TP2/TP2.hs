-- López Garro, Román romanlopezgarro15@gmail.com
-- Martínez, Fernando fernandomch42@gmail.com
-- Ripari, Román Ariel romanripari@gmail.com
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

type Complejo = (Float,Float)

-- 1.1
re :: Complejo -> Float
re z = fst z

-- 1.2
im :: Complejo -> Float
im z = snd z

-- 1.3
suma :: Complejo -> Complejo -> Complejo
suma (a, b) (c, d) = (a + c, b + d)

resta :: Complejo -> Complejo -> Complejo
resta (a, b) (c, d) = (a - c, b - d)

-- 1.4
producto :: Complejo -> Complejo -> Complejo
producto (a, b) (c, d) = ((a * c - b * d), (a * d + b * c))

-- 1.5
conjugado :: Complejo -> Complejo
conjugado (a, 0) = (a, 0)
conjugado (a, b) = (a, -b)

-- 1.6
inverso :: Complejo -> Complejo
inverso z =  ((fst z') / modCuadrado,
              (snd z') / modCuadrado)
 where z' = conjugado z
       modCuadrado = (modulo z) * (modulo z)

-- 1.7
cociente :: Complejo -> Complejo -> Complejo
cociente z w = producto z (inverso w)

-- 1.8
potencia :: Complejo -> Integer -> Complejo
potencia z 1 = z
potencia z k = producto z (potencia z (k-1))

-- Corrección: los cálculos anteriores (donde arrojaba un doble complejo) solo 
-- son correctos cuando el determinante es menor a cero.
-- En el caso de ser mayor o igual a cero, vamos a devolver r1 y r2,
-- que son dos complejos con parte real distinta (por sumar y restar W) y con parte imaginaria cero
-- 1.9
raicesCuadratica :: Float -> Float -> Float -> (Complejo,Complejo)
raicesCuadratica a b c 
 | discriminante < 0 = (z1, z2) 
 | otherwise = (r1, r2)

 where z1 = cociente (suma (-b , 0) (conjugado w) ) (2 * a, 0)     
       z2 = cociente (suma (-b , 0) w) (2 * a, 0) 

       r1 = cociente (suma (-b , 0) w ) (2 * a, 0)     
       r2 = cociente (resta (-b , 0) w) (2 * a, 0) 
       
       w = calculaDosComplejos discriminante 
       discriminante = b * b - 4 * a * c 

calculaDosComplejos :: Float -> Complejo
calculaDosComplejos disc 
 | disc >= 0 = (sqrt disc, 0)
 | disc < 0 =  (0,  sqrt (-disc))


-- 2.1
modulo :: Complejo -> Float
modulo (a, b) = sqrt (a*a + b*b)

-- 2.2
distancia :: Complejo -> Complejo -> Float
distancia z w = modulo (resta z w)

-- Corrección: Corregimos el cálculo de los cuadrantes y quitamos el caso argumento (0,0)
-- 2.3
argumento :: Complejo -> Float 
argumento (a, 0) 
 | a < 0 = pi
 | otherwise = 0 
argumento (0, b)
 | b < 0 = pi * 3/2
 | otherwise = pi * 1/4 
argumento (a, b)
 | a > 0 && b > 0 = θ               -- Cuadrante 1
 | a < 0 && b > 0 = θ + pi          -- Cuadrante 2
 | a < 0 && b < 0 = θ - pi          -- Cuadrante 3
 | a > 0 && b < 0 = θ               -- Cuadrante 4
 where θ = atan(b/a)

-- 2.4
pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r θ = (a, b)
 where a = r * cos θ
       b = r * sin θ

-- Corrección: la condición del signo (iguales o distintos) está
-- dada por la parte imaginaria del número complejo
-- 2.5
raizCuadrada :: Complejo  -> (Complejo,Complejo)
raizCuadrada (a, b)
 | b > 0 = mismoSigno x y  
 | otherwise = distintoSigno x y  
 where x = sqrt ((modulo (a, b) + a ) / 2 )
       y = sqrt ((modulo (a, b) - a ) / 2 )

mismoSigno :: Float -> Float -> (Complejo,Complejo)
mismoSigno x 0 = ((x, 0), (-x, 0))
mismoSigno 0 y = ((0, y), (0, -y))
mismoSigno x y = ((x,y), (-x, -y))

distintoSigno :: Float -> Float -> (Complejo,Complejo)
distintoSigno x 0 = ((x, 0), (-x, 0))
distintoSigno 0 y = ((0, y), (0, -y))
distintoSigno x y = ((x,-y), (-x, y))

-- 2.6
raicesCuadraticaCompleja :: Complejo -> Complejo -> Complejo -> (Complejo,Complejo)
raicesCuadraticaCompleja a b c = (z1, z2)
 where z1 = cociente (resta (fst raices) b) (productoRealComplejo 2 a)     
       z2 = cociente (resta (snd raices) b) (productoRealComplejo 2 a)
       raices = raizCuadrada (resta (potencia b 2) (productoRealComplejo 4 (producto a c)))

productoRealComplejo :: Float -> Complejo -> Complejo
productoRealComplejo real complejo = (real * re complejo, real * im complejo)

-- 3.1
raicesNEsimas :: Integer -> [Complejo]
raicesNEsimas k = raicesNEsimasAux (fromInteger (k-1)) (fromInteger k)
-- Tuvimos que pasar a k como Float, de otra manera el cálculo de 
-- seno y coseno no nos arrojaba un Float sino Integer, y fallaba

raicesNEsimasAux :: Float -> Float -> [Complejo]
raicesNEsimasAux 0 n = [(1,0)]
raicesNEsimasAux k n = raiz : raicesNEsimasAux (k-1) n
 where raiz = (real, imaginario) 
       real = cos ((pi * 2.0 * k ) / n)
       imaginario = sin ((pi * 2.0 * k ) / n)

{-
sonRaicesNEsimas :: Integer -> [Complejo] -> Float -> Bool
sonRaicesNEsimas n [] e = True
sonRaicesNEsimas n zs e
 | calculo >= e = False
 | otherwise = sonRaicesNEsimas n (tail zs) e 
 where calculo = modulo (resta (potencia (head zs) n) (1,0)) 
-}

-- 3.2
-- Forma reducida sin gradas
sonRaicesNEsimas :: Integer -> [Complejo] -> Float -> Bool
sonRaicesNEsimas n [] e = True
sonRaicesNEsimas n zs e = calculo < e && (sonRaicesNEsimas n (tail zs) e )
 where calculo = modulo (resta (potencia (head zs) n) (1,0)) 
