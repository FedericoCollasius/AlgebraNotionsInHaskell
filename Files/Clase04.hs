--Teoría--

sumatoria :: Int -> Int
sumatoria 0 = 0 
sumatoria n = n + sumatoria (n-1)

sumatoria' :: Int -> Int
sumatoria' n = div (n*(n+1)) 2

f1 :: Int -> Int
f1 0 = 1
f1 n = 2^n + f1(n-1)

f1' :: Int -> Int
f1' n = 2^(n+1) -1

f2 :: Int -> Float -> Float 
f2 0 q = 0
f2 n q = q^n + f2 (n-1) q 

f2' :: Int -> Float -> Float
f2' n q | n == 0 = 0
        | otherwise = q^n + f2 (n-1) q

f3 :: Int -> Float -> Float
f3 0 q = 0
f3 n q = (f3 (n-1) q ) + q^(2*n - 1) + q^(2*n)

f3' :: Int -> Float -> Float
f3' n q = f2 (2*n) q

f4 :: Int -> Float -> Float
f4 n q = (f3 n q) - (f2 (n-1) q)

f4' :: Int -> Float -> Float
f4' 0 q = 1
f4' n q = q^(2*n - 1) + q^(2*n) - q^(n-1) + (f4' (n-1) q)

fact :: Int -> Int
fact 1 = 1
fact n = n*(fact (n-1))

eAprox :: Int -> Float
eAprox 0 = 1
eAprox n = (eAprox (n-1)) + 1 / (fromIntegral (fact n)) {-aca sale un error porque la barra de la div espera un 
                                                         real y yo le estoy dando un Int por la definicion del 
                                                         factorial. Lo corrigo con 'fromIntegral' que transforma 
                                                         el numero entero, fact n, en un numero real o float-}

e :: Float
e = eAprox 10 

f :: Int -> Int -> Int
f 0 m = 0
f n m = (f (n-1) m) + round (f2 m (fromIntegral n))

sumaPotencias :: Float -> Int -> Int -> Float
sumaPotencias q n 0 = 0   
sumaPotencias q n m = (sumaPotencias q n (m-1) + q^m * (f2 n q))

sumaRacionales :: Int -> Int -> Float
sumaRacionales n 0 = 0
sumaRacionales n m = (sumaRacionales n (m-1)) + (fromIntegral (sumatoria n)) / (fromIntegral m)


--Ejercicios--

g1 :: Int -> Int -> Int
g1 i 0 = 0
g1 i n | i > n = i^i
       | i == n = i^n
       | otherwise = i^n + g1 i (n-1)

g2 :: Int -> Int
g2 0 = 0
g2 n = g2 (n-1) + aux n n 

aux :: Int -> Int -> Int
aux i 0 = 0
aux i n = n^i + aux i (n-1)

g3 :: Int -> Int
g3 2 = 4 
g3 n |mod n 2 == 0 = 2^n + g3 (n-2)       
     |otherwise = 2^(n+1) + g3((n+1)-2)

digitosIguales :: Int -> Bool
digitosIguales 0 = True
digitosIguales n | div n 10 == 0 = True
                 | mod n 10 == mod (div n 10) 10 = digitosIguales(div n 10)
                 | otherwise = False

sumaDigitosIguales :: Int -> Int
sumaDigitosIguales 0 = 0
sumaDigitosIguales n |digitosIguales n = sumaDigitosIguales (n-1) + n
                     |otherwise = sumaDigitosIguales(n-1)                 
 