{-Teoría-}

module Clase12
where
import Clase07


mcd :: Int -> Int -> Int
mcd a 0 = abs a
mcd a b = mcd b (mod a b)


type Polinomio = [Float]
type Monomio = (Float, Int)


-- 1


limpiar :: [Float] -> Polinomio
limpiar (0:xs) = limpiar xs
limpiar p = p


grado :: Polinomio -> Int
grado [] = undefined
grado [x] = 0
grado (x:xs) = 1 + grado xs


evaluar :: Polinomio -> Float -> Float
evaluar [] _ = 0
evaluar p x = ((head p) * (x^n)) + evaluar (limpiar (tail p)) x
    where n = grado p


-- 2


sumaAux :: Polinomio -> Polinomio -> [Float]
sumaAux [] p = p
sumaAux p [] = p
sumaAux p q = sumaAux (init p) (init q) ++ [(last p) + (last q)]


sumaPolinomio :: Polinomio -> Polinomio -> Polinomio
sumaPolinomio p q = limpiar (sumaAux p q)


-- 3


productoPorEscalar :: Float -> Polinomio -> Polinomio
productoPorEscalar 0 _ = []
productoPorEscalar x [] = []
productoPorEscalar x p = (x * head(p)) : (productoPorEscalar x (tail p))


restaPolinomio :: Polinomio -> Polinomio -> Polinomio
restaPolinomio p q = sumaPolinomio p (productoPorEscalar (-1) q)


productoPorMonomio :: Monomio -> Polinomio -> Polinomio
productoPorMonomio (a, 0) p = productoPorEscalar a p
productoPorMonomio (a, n) p = (productoPorMonomio (a, (n-1)) p) ++ [0]


productoPolinomio :: Polinomio -> Polinomio -> Polinomio
productoPolinomio [] q = []
productoPolinomio p q = sumaPolinomio (productoPorMonomio (head p, grado p) q)  (productoPolinomio (tail p) q)


-- 4


ceros :: Int -> [Float]
ceros 0 = []
ceros n = 0: (ceros (n-1))

 
hacerPolinomio :: Monomio -> Polinomio
hacerPolinomio (a, n) = a : (ceros n)


derivadaMonomio :: Monomio -> Monomio
derivadaMonomio (a, 0) = (0, 0) 
derivadaMonomio (a, n) = (a*fromIntegral(n), n-1)

-- 5


primerCociente :: Polinomio -> Polinomio -> Monomio
primerCociente p q | grado p >= grado q = (head p / head q, grado p - grado q)


primerResto :: Polinomio -> Polinomio -> Polinomio
primerResto p q | grado p >= grado q = restaPolinomio p (productoPorMonomio (primerCociente p q) q)


divisionPolinomio :: Polinomio -> Polinomio -> (Polinomio, Polinomio)
divisionPolinomio p [] = undefined
divisionPolinomio [] q = ([],[])
divisionPolinomio p  q | grado p < grado q = ([], p)
                       | otherwise = (sumaPolinomio (hacerPolinomio (primerCociente p q)) c', r')
          where (c', r') = divisionPolinomio (primerResto p q) q


-- 6


mcdPNM :: Polinomio -> Polinomio -> Polinomio
mcdPNM p [] = p
mcdPNM p q  = mcdPNM q (snd (divisionPolinomio p q))


hacerMonico :: Polinomio -> Polinomio
hacerMonico p | p /= [] = productoPorEscalar (1 / (head p)) p


mcdP :: Polinomio -> Polinomio -> Polinomio
mcdP p q = hacerMonico (mcdPNM p q)


-- 7


type Racional = (Int, Int)


armaR :: Int -> Int -> Racional
armaR num den | den == 0 = undefined
              | den < 0 = armaR (-num) (-den)
              | otherwise = (div num d, div den d)
 where d = mcd num den              


sumaR :: Racional -> Racional -> Racional
sumaR (a, b) (c, d) = armaR (a*d+b*c) (b*d)


multiplicaR :: Racional -> Racional -> Racional
multiplicaR (a, b) (c, d) = armaR (a*c) (b*d) 


potenciaR :: Racional -> Int -> Racional
potenciaR _ 0 = (1, 1)
potenciaR r n = multiplicaR (potenciaR r (n-1)) r


-- 8


type PolinomioZ = [Int]


limpiarZ :: [Int] -> PolinomioZ
limpiarZ (0:xs) = limpiarZ xs
limpiarZ p = p


gradoZ :: PolinomioZ -> Int
gradoZ [] = undefined
gradoZ [x] = 0
gradoZ (x:xs) = 1 + gradoZ xs

evaluarZ :: PolinomioZ -> Racional -> Racional
evaluarZ [] _ = (0, 1)
evaluarZ p x =   sumaR (multiplicaR (head p, 1) (potenciaR x n))  (evaluarZ (limpiarZ (tail p)) x)
    where n = gradoZ p


esRaizRacional :: PolinomioZ -> Racional -> Bool
esRaizRacional p r = evaluarZ p r == (0,1)


raicesRacEnConjunto :: PolinomioZ -> Set Racional -> Set Racional
raicesRacEnConjunto p [] = []
raicesRacEnConjunto p (x:xs) | esRaizRacional p x = x : raicesRacEnConjunto p xs
                             | otherwise = raicesRacEnConjunto p xs
                    

-- 9


divisores :: Int -> Set Int
divisores n = divisoresHastaR n (abs n)


divisoresHastaR :: Int -> Int -> Set Int
divisoresHastaR n 1 = [1, -1]
divisoresHastaR n k | mod n k == 0 = [k, -k] ++ divisoresHastaR n (k-1)
                   | otherwise = divisoresHastaR n (k-1)


divisoresPos :: Int -> Set Int
divisoresPos n = divisoresPosHasta n (abs n)


divisoresPosHasta :: Int -> Int -> Set Int
divisoresPosHasta n 1 = [1]
divisoresPosHasta n k | mod n k == 0 = k:divisoresPosHasta n (k-1)
                      | otherwise = divisoresPosHasta n (k-1)

agregarRac :: Racional -> Set Racional -> Set Racional
agregarRac r c | elem r c = c
               | otherwise = r:c

armarSetRac :: Set (Int, Int) -> Set Racional
armarSetRac [] = []
armarSetRac ((num, den):xs) = agregarRac (armaR num den) (armarSetRac xs)


candidatosRaices :: PolinomioZ -> Set Racional
candidatosRaices [] = undefined
candidatosRaices p | last p == 0 = agregarRac (0,1) (candidatosRaices (init p))
                   | otherwise = armarSetRac (productoCartesiano (divisores (last p)) (divisoresPos (head p)))


raicesRacionales :: PolinomioZ -> Set Racional
raicesRacionales p = raicesRacEnConjunto p (candidatosRaices p)

{-Práctica-}

hacerMonomios :: Polinomio -> [Monomio]
hacerMonomios [] = []
hacerMonomios (p:ps)| p /= 0 = (p, grado (p:ps)) : (hacerMonomios ps)
                    | otherwise = hacerMonomios ps 

sumaMonomios :: [Monomio] -> Polinomio
sumaMonomios [] = []
sumaMonomios (m:ms)  = sumaPolinomio (hacerPolinomio m) (sumaMonomios ms)

derivarCadaMonomio :: [Monomio] -> [Monomio]
derivarCadaMonomio [] = []
derivarCadaMonomio (m:ms)  = (derivadaMonomio m) : (derivarCadaMonomio ms)

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