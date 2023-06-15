module ProgramasUtilesParaAlgebra
where
import Clase05 (menorDivisor,menorDivisorDesde,minimoPrimoDesde,sumaDivisoresHasta)

import Clase07 (partes,agregarATodos,unionC,agregarC,perteneceC,incluido,iguales,pertenece,vacio,productoCartesiano_aux,unionTup,
                agregarTup,perteneceTup)

import Clase08 (listasOrdenadas,listasOrdenadas',filtrarTam,insertarEnCadaPosDeTodasLasListas,insertarEnCadaPos,insertarEn,variaciones,
                agregarElementosAListas,agregarElementosAdelante)

import Clase08Adicional (codificarAListaDesde,godelDesde)

import Clase10 (solucSistemaModCoprimos,sistemaEquivSinPrimosMalos,sistemaEquivSinPrimosMalosAux,desdoblarSistemaEnFcionPrimo,
                solucSistemaPotenciasPrimo,solucDosEcPotenciasPrimo,solucDosEcPotenciasPrimoOrd,todosLosPrimosMalos,todosLosPrimosMalosHasta,
                esPrimoMalo,cantidadMultiplos,cotaParaPrimoMalo,cotaParaPrimoMaloDesde,mayorModulo,modulos,sistemaSimplifEquiv,
                solucionEcConPropAdic,ecEquivalente)

import Clase11 (re,im,conjugadoComplejos,sumaComplejos,restaComplejos,productoComplejos,inversoComplejos,moduloComplejos,cuadrante,
                pasarACartesianas,raicesNEsimasDesde)

import Clase12 (limpiar,grado,sumaAux,sumaPolinomio,productoPorEscalar,restaPolinomio,productoPorMonomio,ceros,hacerPolinomio,
	             derivadaMonomio,primerCociente,primerResto,mcdPNM,hacerMonico,candidatosRaices,armarSetRac,agregarRac,divisoresPosHasta,
                 divisores,divisoresHastaR,raicesRacEnConjunto,gradoZ,limpiarZ,potenciaR,multiplicaR,sumaR,armaR,hacerMonomios,
                 derivarCadaMonomio,sumaMonomios,)


--Clase 01
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | x <= 0 = undefined 
                 | y <= 0 = undefined 
                 | mod x y == 0 = True
                 | otherwise = False

--Clase 03
potenciaOtro :: Int -> Int -> Bool 
potenciaOtro 1 m = True
potenciaOtro 0 m = False
potenciaOtro n m |mod n m == 0 = potenciaOtro (div n m) m
                 |otherwise = False

--Clase 04
factorial :: Int -> Int
factorial 1 = 1
factorial n = n*(factorial (n-1))


sumatoria :: Int -> Int
sumatoria 0 = 0 
sumatoria n = n + sumatoria (n-1)

--Clase 05
productoria :: Int -> Int -> Int
productoria d h   | d == h = d
prodoductoria d h | otherwise = h * productoria d (h-1)


esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n 


nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n


--Clase 06
sumatoriaLista :: [Int] -> Int
sumatoriaLista l | l == [] = 0
                 | otherwise = head l + sumatoriaLista (tail l)


productoriaLista :: [Int] -> Int
productoriaLista [] = 1 
productoriaLista l  = head l * productoriaLista (tail l) 

--Clase 07
type Set a = [a]


partesN :: Int -> Set (Set Int)
partesN 0 = [[]]
partesN n = unionC (partesN (n-1)) (agregarATodos n (partesN (n-1)))


productoCartesiano :: Set Int -> Set Int -> Set (Int,Int)  
productoCartesiano (_) [] = []
productoCartesiano [] (_) = []
productoCartesiano (x:xs) (y:ys) = unionTup (productoCartesiano_aux x (y:ys)) (productoCartesiano xs (y:ys))

                
--Clase 08
combinatorio :: Int -> Int -> Int 
combinatorio n k = factorial n `div` ((factorial k) * (factorial (n - k )))

permutaciones :: Set Int -> Set [Int]
permutaciones [] = [[]]
permutaciones (c:cs) = insertarEnCadaPosDeTodasLasListas (permutaciones cs) c


subconjuntos :: Set Int -> Int -> Set (Set Int)
subconjuntos c x = listasOrdenadas' c x 


--Clase 08 Adicional
quePotenciaLoDivide :: Integer -> Integer -> Integer
quePotenciaLoDivide n p | mod n p == 0 = 1 + quePotenciaLoDivide (div n p) p 
                        | otherwise = 0 

codificarALista :: Integer -> [Integer] 
codificarALista n = codificarAListaDesde n 1

godel :: [Integer] -> Integer
godel (x:xs) = godelDesde (x:xs) 1


--Clase 09
digitos :: Integer -> Integer -> [Integer]
digitos 0 b = []
digitos n b = n `mod` b : digitos (n `div` b) b 


numero :: [Integer] -> Integer -> Integer 
numero [] b = 0
numero (x:xs) b = x + (b* (numero (xs) b ))


mcd :: Int -> Int -> Int 
mcd _ 1 = 1
mcd a b | a `mod` b == 0 = b 
        | otherwise = mcd b (a `mod` b)


mcm :: Int -> Int -> Int 
mcm a b = (a*b) `div` mcd a b


emcd :: Int -> Int -> (Int , Int , Int)
emcd a 0 = (a, 1, 2)
emcd a b =  (d, t, s - (div a b)*t)
    where 
         (d,s,t) = emcd b (mod a b)

--Clase 10
solucionEc :: (Int, Int, Int) -> (Int, Int)
solucionEc e = solucionEcConPropAdic (ecEquivalente e)

solucSistema :: [(Int, Int, Int)] -> (Int, Int)
solucSistema sist = solucSistemaModCoprimos ( sistemaEquivSinPrimosMalos ( sistemaSimplifEquiv sist) )

--Clase 11
type Complejo = (Float,Float)


argumento :: Complejo -> Float 
argumento (a,b) |cuadrante (a,b) == 1 = atan (b/a)
                |cuadrante (a,b) == 2 = pi + (atan (b/a))
                |cuadrante (a,b) == 3 = pi + (atan (b/a))
                |otherwise = 2*pi + (atan (b/a))


raizCuadradaComplejos :: Complejo -> (Complejo,Complejo)
raizCuadradaComplejos z = (pasarACartesianas r tita , pasarACartesianas r (tita + pi))
                  where r    = sqrt (moduloComplejos z)
                        tita = (argumento z)/2


raicesNEsimas :: Integer -> [Complejo]
raicesNEsimas n = raicesNEsimasDesde 0 n


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


--Clase 12
type Polinomio = [Float]
type Monomio = (Float, Int)
type Racional = (Int, Int)
type PolinomioZ = [Int]


derivadaPolinomio :: Polinomio -> Polinomio
derivadaPolinomio [] = []
derivadaPolinomio p  = sumaMonomios (derivarCadaMonomio(hacerMonomios p))


multiplicidad :: Float -> Polinomio -> Int
multiplicidad n [] = 0 
multiplicidad n p | evaluar p n == 0 = 1 + multiplicidad n (derivadaPolinomio p)
                  | otherwise = 0


raicesMultiples :: Polinomio -> Bool
raicesMultiples p = not (mcdP p (derivadaPolinomio p) == [1.0])  


derivadaNesima :: Polinomio -> Int -> Polinomio
derivadaNesima [] n = []
derivadaNesima p 0 = p 
derivadaNesima p n = derivadaNesima (derivadaPolinomio p) (n-1)


evaluar :: Polinomio -> Float -> Float
evaluar [] _ = 0
evaluar p x = ((head p) * (x^n)) + evaluar (limpiar (tail p)) x
    where n = grado p         	


productoPolinomio :: Polinomio -> Polinomio -> Polinomio
productoPolinomio [] q = []
productoPolinomio p q = sumaPolinomio (productoPorMonomio (head p, grado p) q)  (productoPolinomio (tail p) q)


divisionPolinomio :: Polinomio -> Polinomio -> (Polinomio, Polinomio)
divisionPolinomio p [] = undefined
divisionPolinomio [] q = ([],[])
divisionPolinomio p  q | grado p < grado q = ([], p)
                       | otherwise = (sumaPolinomio (hacerPolinomio (primerCociente p q)) c', r')
          where (c', r') = divisionPolinomio (primerResto p q) q


mcdP :: Polinomio -> Polinomio -> Polinomio
mcdP p q = hacerMonico (mcdPNM p q)


raicesRacionales :: PolinomioZ -> Set Racional
raicesRacionales p = raicesRacEnConjunto p (candidatosRaices p)


divisoresPos :: Int -> Set Int
divisoresPos n = divisoresPosHasta n (abs n)


evaluarZ :: PolinomioZ -> Racional -> Racional
evaluarZ [] _ = (0, 1)
evaluarZ p x =   sumaR (multiplicaR (head p, 1) (potenciaR x n))  (evaluarZ (limpiarZ (tail p)) x)
    where n = gradoZ p


esRaizRacional :: PolinomioZ -> Racional -> Bool
esRaizRacional p r = evaluarZ p r == (0,1)

--TP 1
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = noExisteDivisorComunDesde a b 2 


noExisteDivisorComunDesde :: Integer -> Integer -> Integer -> Bool
noExisteDivisorComunDesde a b n |n > a || n > b = True
                                | otherwise     = not (mod a n == 0 && mod b n == 0) && noExisteDivisorComunDesde a b (n+1)


esAPseudoprimo :: Integer -> Integer -> Bool
esAPseudoprimo n a = n > 1 && not (esPrimo (fromIntegral n)) && mod ((a^(n-1)) - 1) n == 0 


--TP 2
factorizacionEnPrimos :: Integer -> [Integer]
factorizacionEnPrimos n = factorizacionEnPrimos' n cotaDivisorPrimoN
     where
          cotaDivisorPrimoN = proximoPrimoMenorOIgualDesde (round (sqrt (fromIntegral n)))


factorizacionEnPrimos' :: Integer -> Integer -> [Integer]
factorizacionEnPrimos' 1 p = []
factorizacionEnPrimos' n p | esPrimo (fromIntegral n) = [n]
                           | mod n p == 0 = p : factorizacionEnPrimos' (div n p) p
                           | otherwise = factorizacionEnPrimos'  n (proximoPrimoMenorDesde p)


proximoPrimoMenorDesde :: Integer -> Integer
proximoPrimoMenorDesde 2 = 2
proximoPrimoMenorDesde n | esPrimo (fromIntegral(n - 1)) = n - 1 
                         | otherwise = proximoPrimoMenorDesde (n - 1)


proximoPrimoMenorOIgualDesde :: Integer -> Integer 
proximoPrimoMenorOIgualDesde 2 = 2 
proximoPrimoMenorOIgualDesde n | esPrimo(fromIntegral n) = n
                               | otherwise = proximoPrimoMenorDesde (n - 1)                           