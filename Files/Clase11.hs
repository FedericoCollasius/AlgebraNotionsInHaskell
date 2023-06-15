module Clase11 
where
	
{-Teoría-}

type Complejo = (Float,Float)

re :: Complejo -> Float
re (a,_) = a

im :: Complejo -> Float
im (_,b) = b

conjugadoComplejos :: Complejo -> Complejo
conjugadoComplejos (a,b) = (a,-b)

sumaComplejos :: Complejo -> Complejo -> Complejo
sumaComplejos (a,b) (c,d) = (a+c,b+d)

restaComplejos :: Complejo -> Complejo -> Complejo
restaComplejos (a,b) (c,d) = (a-c,b-d)

productoComplejos :: Complejo -> Complejo -> Complejo
productoComplejos (a,b) (c,d) = (a*c-b*d,a*d+b*c)

inversoComplejos :: Complejo -> Complejo
inversoComplejos (a,b) = (a/(a**2 + b**2),(-b)/(a**2 + b**2))

moduloComplejos :: Complejo -> Float
moduloComplejos (a,b) = sqrt(a**2 + b**2)

argumento :: Complejo -> Float 
argumento (a,b) |cuadrante (a,b) == 1 = atan (b/a)
                |cuadrante (a,b) == 2 = pi + (atan (b/a))
                |cuadrante (a,b) == 3 = pi + (atan (b/a))
                |otherwise = 2*pi + (atan (b/a))

cuadrante :: Complejo -> Int
cuadrante (a,b) |a >= 0 && b >= 0 = 1
                |a <= 0 && b >= 0 = 2
                |a <= 0  && b <= 0 = 3
                |a >= 0 && b <= 0 = 4

pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r tita = (r*(cos tita),r*(sin tita))

raizCuadradaComplejos :: Complejo -> (Complejo,Complejo)
raizCuadradaComplejos z = (pasarACartesianas r tita , pasarACartesianas r (tita + pi))
                  where r    = sqrt (moduloComplejos z)
                        tita = (argumento z)/2

raicesNEsimas :: Integer -> [Complejo]
raicesNEsimas n = raicesNEsimasDesde 0 n 

raicesNEsimasDesde :: Integer -> Integer -> [Complejo]
raicesNEsimasDesde k n | k >= n = []
                       | otherwise =(kesimaRaiz):(raicesNEsimasDesde (k+1) n)
            where 
            	kesimaRaiz = (cos (2*(fromInteger k)*pi/(fromInteger n)) , sin ((2*(fromInteger k)*pi)/(fromInteger n)))

{-Práctica-}

cocienteComplejos :: Complejo -> Complejo -> Complejo
cocienteComplejos (a,b) (c,d) = (((a*c + b*d)/ difcuadrados), ((b*c - a*d) / difcuadrados))
	where
		difcuadrados = c**2 + d**2

potenciaComplejos :: Complejo -> Integer -> Complejo 
potenciaComplejos (_ , _) 0 = (1, 0)
potenciaComplejos (a, b) n = productoComplejos (a, b) (potenciaComplejos (a,b) (n - 1))

solucionesCuadratica :: Float -> Float -> Float -> (Complejo, Complejo)
solucionesCuadratica a b c | discriminante >= 0 = (((-b + discriminante') / a', 0),((-b - discriminante') / a',0))
	                       | otherwise =  ((cocienteComplejos (-b, discriminante') (a', 0)), (cocienteComplejos (-b, -discriminante') (a',0)))                 
	where                  
		discriminante = b*b - 4*a*c
		a' = 2*a
		discriminante' = sqrt (abs (discriminante)) 

solucionesCuadraticaCoefComplejos :: Complejo -> Complejo -> Complejo -> (Complejo, Complejo)
solucionesCuadraticaCoefComplejos (a,b) (c,d) (e,f) = (solucion1, solucion2)
	where 
        (x1,x2) = raizCuadradaComplejos (restaComplejos (potenciaComplejos (c,d) 2) (productoComplejos (4*a,4*b) (e,f)))  
        solucion1 = cocienteComplejos (restaComplejos (-c,-d) x1) (2*a, 2*b)
        solucion2 = cocienteComplejos (restaComplejos (-c,-d) x2) (2*a, 2*b)

potenciasRaizNEsima :: Integer -> Integer -> [Complejo]
potenciasRaizNEsima k n | n == 1 = [(0, 0)]
                        | otherwise = (potenciaComplejos kesimaRaiz n) : potenciasRaizNEsima k (pred n)
       where
          kesimaRaiz = (cos (2*(fromInteger k)*pi/(fromInteger n)) , sin ((2*(fromInteger k)*pi)/(fromInteger n)))         



