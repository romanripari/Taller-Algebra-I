-- Apellido Nombre email #1
-- Apellido Nombre email #2
-- Apellido Nombre email #3
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

type Complejo = (Float,Float)

c1 :: Complejo
c1 = (10.0, 5.0)
c2 :: Complejo
c2 = (5.0, 3.0)

re :: Complejo -> Float
re z = fst z

im :: Complejo -> Float
im z = snd z

suma :: Complejo -> Complejo -> Complejo
suma (a, b) (c, d) = (a + c, b + d)

producto :: Complejo -> Complejo -> Complejo
producto (a, b) (c, d) = ((a * c - b * d), (a * d + b * c))

conjugado :: Complejo -> Complejo
conjugado (a, 0) = (a, 0)
conjugado (a, b) = (a, -b)

modulo :: Complejo -> Float
modulo (a, b) = sqrt (a*a + b*b)

inverso :: Complejo -> Complejo
inverso z =  ((fst z') / modCuadrado,
              (snd z') / modCuadrado)
 where z' = conjugado z
       modCuadrado = (modulo z) * (modulo z)

cociente :: Complejo -> Complejo -> Complejo
cociente z z' = producto z (inverso z')

potencia :: Complejo -> Integer -> Complejo
potencia z 1 = z
potencia z k = producto z (potencia z (k-1))

raicesCuadratica :: Float -> Float -> Float -> (Complejo,Complejo)
raicesCuadratica a b c = (z1, z2)
 where z1 = cociente (suma (-b , 0) (conjugado w) ) (2 * a, 0)     
       z2 = cociente (suma (-b , 0) w) (2 * a, 0) 
       w = calculaDeterminante (b*b - 4 * a * c)

calculaDeterminante :: Float -> Complejo
calculaDeterminante a 
 | a >= 0 =  (sqrt a, 0)
 | a < 0 =  (0,  sqrt (-a))

opuesto :: Complejo -> Complejo
opuesto (a, b) = (-a, -b) 

distancia :: Complejo -> Complejo -> Float
distancia z w = modulo (suma z (opuesto w))

argumento :: Complejo -> Float 
argumento (0, 0) = 0
argumento (a, 0) 
 | a < 0 = pi
 | otherwise = 0 
argumento (0, b)
 | b < 0 = pi * 3/2
 | otherwise = pi * 1/4 
argumento (a, b)
 | a > 0 && b > 0 = θ
 | a < 0 && b < 0 = θ + pi
 | a > 0 && b < 0 = θ + pi * 1/2
 | a < 0 && b > 0 = θ + pi * 3/2
 where θ = atan(b/a)

pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r θ = (a, b)
 where a = r * cos θ
       b = r * sin θ

raizCuadrada :: Complejo -> (Complejo,Complejo)
raizCuadrada (a, b)
 | calculo > 0 = mismoSigno x y  
 | otherwise = distintoSigno x y  
 where calculo = 2 * x * y
       x = sqrt ((modulo (a, b) + a ) / 2 )
       y = sqrt ((modulo (a, b) - a ) / 2 )

mismoSigno :: Float -> Float -> (Complejo,Complejo)
mismoSigno x 0 = ((x, 0), (-x, 0))
mismoSigno 0 y = ((0, y), (0, -y))
mismoSigno x y = ((x,y), (-x, -y))

distintoSigno :: Float -> Float -> (Complejo,Complejo)
distintoSigno x 0 = ((x, 0), (-x, 0))
distintoSigno 0 y = ((0, y), (0, -y))
distintoSigno x y = ((x,-y), (-x, y))

productoRealComplejo :: Float -> Complejo -> Complejo
productoRealComplejo real complejo = (real * re complejo, real * im complejo)


raicesCuadraticaCompleja :: Complejo -> Complejo -> Complejo -> (Complejo,Complejo)
raicesCuadraticaCompleja a b c = (z1, z2)
 where z1 = cociente (suma (opuesto b) (fst raices)) (productoRealComplejo 2 a)     
       z2 = cociente (suma (opuesto b) (snd raices)) (productoRealComplejo 2 a)
       raices = raizCuadrada (suma (potencia b 2) (opuesto (productoRealComplejo 4 (producto a c))))

raicesNEsimas :: Integer -> [Complejo]
raicesNEsimas k = raicesNEsimasAux (fromInteger (k-1)) (fromInteger k)
-- Tuvimos que pasar a k como Float, de otra manera el cálculo de 
-- seno y coseno no nos arrojaba un Float sino Integer

raicesNEsimasAux :: Float -> Float -> [Complejo]
raicesNEsimasAux 0 n = [(1,0)]
raicesNEsimasAux k n = raiz : raicesNEsimasAux (k-1) n
 where raiz = (real, imaginario) 
       real = cos ((pi * 2.0 * k ) / n)
       imaginario = sin ((pi * 2.0 * k ) / n)


sonRaicesNEsimas :: Integer -> [Complejo] -> Float -> Bool
sonRaicesNEsimas n [] e = True
sonRaicesNEsimas n zs e
 | calculo >= e = False
 | otherwise = sonRaicesNEsimas n (tail zs) e 
 
 where calculo = modulo ( suma (potencia (head zs) n) (opuesto (1,0)) ) 
