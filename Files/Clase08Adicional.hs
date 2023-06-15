module Clase08Adicional 
where 
	
{-Práctica-}

--Funciones de otras clases: 

minimoPrimoDesde :: Integer -> Integer
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n+1)

esPrimo :: Integer -> Bool
esPrimo n = (n > 1) && not (tieneDivisoresDesde n 2)

tieneDivisoresDesde :: Integer -> Integer -> Bool
tieneDivisoresDesde n k | k == n = False
                        | otherwise = (mod n k == 0) || tieneDivisoresDesde n (k+1)

nEsimoPrimo :: Integer -> Integer 
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))


-- Longitud n, dado un número natural n, calcula la longitud de la lista que codifica longitud
-- 132 = 5.

longitud :: Integer -> Integer
longitud n = longitudDesde n 1 

longitudDesde :: Integer -> Integer -> Integer 
longitudDesde 1 _ = 0 
longitudDesde n k = 1 + longitudDesde (div n (p^a)) (k + 1)
        where
        	p = nEsimoPrimo k 
        	a = quePotenciaLoDivide n p

quePotenciaLoDivide :: Integer -> Integer -> Integer
quePotenciaLoDivide n p | mod n p == 0 = 1 + quePotenciaLoDivide (div n p) p 
                        | otherwise = 0 

-- iesimo n i, dados dos números naturales n e i, devuelve el iésimo elemento de la lista
-- que codifica n. 
-- Ej: iesimo 132 2 --> 1. El primer elemento de la lista se corresponde con el índice 1.
-- Si el índice está fuera de rango, el programa devuelve 0. 

iesimo :: Integer -> Integer -> Integer
iesimo n i = quePotenciaLoDivide n (nEsimoPrimo i)


-- headN, dado un número natural n, devuelve la cabeza de la lista codificada por n. 
-- Ej: headN 132 --> 2 

headN :: Integer -> Integer
headN n = quePotenciaLoDivide n 2 

-- tailN, dado un número natural n, devuelve el número que codifica a la cola de la lista
-- que codifica n.
-- Ej: tailN 132 -> tailN (2^2 . 3^1 . 5^0 . 7^0 . 11^1) = 2^1 . 3^0 . 5^0 . 7^1 = 14

tailN :: Integer -> Integer 
tailN n = tailNDesde n 1

tailNDesde :: Integer -> Integer -> Integer
tailNDesde 1 _ = 1
tailNDesde n k = (p1^b) * tailNDesde (div n (p1^a)) (k + 1) 
	where
		p1 = nEsimoPrimo k 
		pn = nEsimoPrimo (k + 1)
		a = quePotenciaLoDivide n p1
		b = quePotenciaLoDivide n pn 

-- codificarALista, dado un número natural, devuelve la lista a la que codifica. 

codificarALista :: Integer -> [Integer] 
codificarALista n = codificarAListaDesde n 1

codificarAListaDesde :: Integer -> Integer -> [Integer]
codificarAListaDesde 1 _ = []
codificarAListaDesde n k = a : codificarAListaDesde (div n (p^a)) (k + 1)
         where
         	a = iesimo n k 
         	p = nEsimoPrimo k 

-- godel, dada una lista l, devuelve el número de Godel de l.

godel :: [Integer] -> Integer
godel (x:xs) = godelDesde (x:xs) 1

godelDesde :: [Integer] -> Integer -> Integer
godelDesde [] _ = 1 
godelDesde (x:xs) k = (a^x) * godelDesde xs (k + 1)
       where
       	a = nEsimoPrimo k 