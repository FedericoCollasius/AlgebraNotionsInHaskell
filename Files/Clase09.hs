module Clase09 where 
type Set a = [a]


{-
PARTE 1: Algoritmo de División.
-}

--Teoría:

-- |Division  de  numeros  naturales_0: a`divNat`d = a`div`d
divNat  :: Int  -> Int  -> Int
divNat a d | a < d = 0
           | otherwise = (a-d)`divNat`d + 1

-- |Resto  de  numeros  naturales_0: a`modNat`d = a`mod`d
modNat  :: Int  -> Int  -> Int
modNat a d = a - d*(a`divNat`d)

-- |Modulo  de  numeros  enteros a`modulo`d = a`mod`d
modulo  :: Int  -> Int  -> Int
modulo a d | a  >= 0 || r'== 0 = r'
           | otherwise = abs d - r'
    where r'= abs a`modNat`abs d

-- |Division  de  numeros  enteros: n`dividido`m = n`div`m
dividido  :: Int  -> Int  -> Int
dividido a d = sgq * absq                      --obs 1
        where  absq = abs (a-r)`divNat`(abs d) --obs 2
               sgq = (signum a) * (signum d)   --obs 3
               r = a`modulo`d

{-
PARTE 2: Sistemas de Numeración.
-}

--Ejercícios:

-- 1) Dados n >= 0 y b > 1, devuelve su representación por listas 
--    en base b.
digitos :: Integer -> Integer -> [Integer]
digitos 0 b = []
digitos n b = n `mod` b : digitos (n `div` b) b 

-- 2) Dada la representación por listas de n >= 0 en base b y la
--    base b > 1, retorne n.
numero :: [Integer] -> Integer -> Integer 
numero [] b = 0
numero (x:xs) b = x + (b* (numero (xs) b ))

{-
PARTE 3: Algoritmo de Euclides.
-}

--Ejercicios:

-- 3) Dado un valor n /= 0 retorna el conjunto de divisores.
--    positivos.
divisores :: Int -> Set Int 
divisores 1 = [1]
divisores n = menorDivisorDesde' n n 

menorDivisorDesde' :: Int -> Int -> Set Int 
menorDivisorDesde' n k | k == 1 = [] 
                       | mod n k == 0 = k : menorDivisorDesde' n (k-1)
                       | otherwise = menorDivisorDesde' n (k-1)

-- 4) Completar la funcion mcdDef, definiendo las funciones restantes.
mcdDef  :: Int -> Int -> Int
mcdDef a 0 = abs a
mcdDef 0 b = abs b
mcdDef a b = maximo (interseccion (divisores a) (divisores b))

interseccion :: Set Int -> Set Int -> Set Int
interseccion (_) [] = []
interseccion [] (_) = []
interseccion (x:xs) (y:ys) | pertenece x (y:ys) = x : interseccion (xs) (y:ys)
                           | otherwise = interseccion (xs) (y:ys)

pertenece :: Int -> Set Int -> Bool
pertenece _ []                 = False
pertenece x (y:ys) | x == y    = True
                   | otherwise = pertenece x ys

maximo :: [Int] -> Int 
maximo (s:xs)  | xs == []     = s
               | s > head xs  = maximo (s : tail(xs))
               | otherwise    = maximo xs

-- 6) Dados a,b ∈ Z, b /= 0, calcule (a:b) usando el algoritmo de Euclides.
mcd :: Int -> Int -> Int 
mcd _ 1 = 1
mcd a b | a `mod` b == 0 = b 
        | otherwise = mcd b (a `mod` b)

-- 8) Dados a ≥ 0 y b  ≥ 0 calcule el mínimo m ≥ 0 que sea múltiplo tanto de a como de b.  
mcm :: Int -> Int -> Int 
mcm a b = (a*b) `div` mcd a b  

{-
PARTE 4: Algoritmo de Euclides Extendido.
-}

--Ejercicios:

-- 9) Dados a y b, utilice el algoritmo de Euclides extendido para obtener una
--    tripla ((a : b), s, t) tal que sa + tb = (a : b)
emcd :: Int -> Int -> (Int , Int , Int)
emcd a 0 = (a, 1, 2)
emcd a b =  (d, t, s - (div a b)*t)
    where 
         (d,s,t) = emcd b (mod a b)

-- 10) Definir una función que dados a /= 0 y b/= 0 encuentre el par s,t ∈ Z tal 
--     que sa + tb = (a:b) donde s ≥ 0 sea lo mínimo posible.  
--     Repasar la teórica para este ejercicio.


          





