module Clase05 where 

--Teoría--

fact :: Int -> Int 
fact 0 = 1
fact n = n * fact (n-1)

prod :: Int -> Int -> Int
prod d h | d == h = d
prod d h | otherwise = h * prod d (h-1)

fact' :: Int -> Int
fact' n = prod 1 n

prod' :: Int -> Int -> Int
prod' d h | d == h  = d
prod' d h | otherwise = d * (prod' (d+1) h)

sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n k | k == 1 = 1
                       | mod n k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise	  = sumaDivisoresHasta n (k-1)	

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n 

sumaDivisoresDesde :: Int -> Int -> Int
sumaDivisoresDesde n k | k == n = n
					   | mod n k == 0 = k + sumaDivisoresDesde n (k+1)
					   | otherwise    = sumaDivisoresDesde n (k+1)

sumaDivisores':: Int -> Int
sumaDivisores' n = sumaDivisoresDesde n 1

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2 

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise    = menorDivisorDesde n (k+1)

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n 

esPrimo' :: Int -> Bool
esPrimo' n = (n > 1) && not (tieneDivisoresDesde n 2)

tieneDivisoresDesde :: Int -> Int -> Bool
tieneDivisoresDesde n k | k == n = False
                        | otherwise = (mod n k == 0) || tieneDivisoresDesde n (k+1)

nEsimoPrimo :: Int -> Int 
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n+1)

menorFactDesde :: Int -> Int
menorFactDesde m = menorFactDesdeDesde 1 m 

menorFactDesdeDesde :: Int -> Int -> Int
menorFactDesdeDesde i m | (fact i) >= m = fact i 
                        | otherwise = menorFactDesdeDesde (i + 1) m


--Ejercicios--

{-Ejercicio 7:
	Implementar mayorFactHasta :: Int -> Int que dado m >= 1 
	encuentra el máximo n <= m tal que n = k! para algun k. 
-}
mayorFactHasta :: Int -> Int
mayorFactHasta m = mayorFactHastaHasta 1 m

mayorFactHastaHasta :: Int -> Int -> Int
mayorFactHastaHasta i m |(fact i) > m = fact (i-1)
                        |otherwise = mayorFactHastaHasta ( i + 1) m 

{-Ejercicio 8:
	Implementar esFact :: Int -> Bool que dado n >= 0 decide si
	existe un número entero k >= 0 tal que n = k! . 
-}
esFact :: Int -> Bool
esFact n | n == repFact n 1 = True
         | otherwise = False

repFact :: Int -> Int-> Int 
repFact n i | n == fact i = n  
            | n > fact i = repFact n (i + 1)
            | otherwise = fact (i-1)

{-Ejercicio 9
	Implementar esFibonacci :: Int -> Bool que dado un número 
	enterno n >= 0 decide si es un número de Fibonacci.
-}
esFibo :: Int -> Bool 
esFibo n | n == repFibo n 0 = True
              | otherwise = False

repFibo :: Int -> Int -> Int
repFibo n i | n == fibo i = n 
            | n > fibo i = repFibo n (i + 1)
            | otherwise = fibo (i - 1)

fibo :: Int -> Int
fibo n | n <= 1 = 1
       | otherwise = fibo (n-1) + fibo (n-2)

{-Ejercico 10 
	Implementar esSumaInicalDePrimos :: Int -> Bool que dado un 
	número entero n >= 0 deide si n es igual a la suma de los m
	primeros números primos, para algún m. 
-}
esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n | 0 == restaDePrimos n 1 = True
                        | otherwise = False

restaDePrimos :: Int -> Int -> Int
restaDePrimos n i | n - p i > 0 = restaDePrimos (n - p i) (i + 1)
                  | n - p i == 0 = 0 
                  | otherwise = 1 


{-Ejercicio 11:
	Implementar tomaValorMax :: Int -> Int -> Int que dado un
	número entero n1 >= 1 y un n2 >= n1 devuelve algún m entre
	entre n1 y n2 tal que:
	sumaDivisores (m) = max{sumaDivisores(i)/n1 <= i <= n2}
-}
tomaValorMax :: Int -> Int -> Int 
tomaValorMax n1 n2 = tomaValorMax_aux n1 n2 n1 n1

tomaValorMax_aux :: Int -> Int -> Int -> Int -> Int 
tomaValorMax_aux n1 n2 i m | sumaDivisores (m) >= sumaDivisores (n2) = m 
                           | sumaDivisores (m) > sumaDivisores (i) = tomaValorMax_aux n1 n2 (i+1) m 
                           | sumaDivisores (m) < sumaDivisores (i) = tomaValorMax_aux n1 n2 (i+1) i 
                           | sumaDivisores (m) == sumaDivisores (i) = tomaValorMax_aux n1 n2 (i+1) m


{- Ejercicio 12:
	Implementar tomaValorMin :: Int -> Int -> Int que dado
	un número entero n1 >= 1 y un n2 >= n1 devuelve algún m
	entre n1 y n2 tal que:
	sumaDivisores (m) = min{sumaDivisores (i)| n1 <= i <= n2}
-} 
tomaValorMin :: Int -> Int -> Int 
tomaValorMin n1 n2 = tomaValorMin_aux n1 n2 n1 n1

tomaValorMin_aux :: Int -> Int -> Int -> Int -> Int 
tomaValorMin_aux n1 n2 i m | sumaDivisores (m) <= sumaDivisores (n2) = m 
                           | sumaDivisores (m) < sumaDivisores (i) = tomaValorMin_aux n1 n2 (i+1) m 
                           | sumaDivisores (m) > sumaDivisores (i) = tomaValorMin_aux n1 n2 m i 
                           | sumaDivisores (m) == sumaDivisores (i) = tomaValorMin_aux n1 n2 (i+1) m                  

{-Ejercicio 13: 
	Implementar esSumaDeDosPrimos :: Int -> Bool que, dado un
	número natural n, determine si puede escribirse como suma
	de dos números primos. 
-}
esSumaDeDosPrimos :: Int -> Bool
esSumaDeDosPrimos n | True == recPrimos n 1 n  = True
                    | otherwise = False

recPrimos :: Int -> Int -> Int -> Bool
recPrimos n a1 k |n <= 3 = False
                 |div k 2 > n = False
                 |esPrimo (n-1) && esPrimo (a1) = True
                 |otherwise = recPrimos (n-1) (a1+1) k
        
{-Ejercicio 14: Conjetura de Christian Goldbach
	Todo número par mayor que 2 puede escribirse como suma de 
	dos números primos. Escribir una función que prueba la
	conjetura hasta un cierto punto (4.10^18)
-}
goldbach :: Int -> Bool
goldbach n | n == 4 = True
           |esSumaDeDosPrimos n == True = goldbach (n-2)
           |otherwise = False
                    
{-Ejercicio 15:
	Los números naturales a y b forman un par de primos gemelos
	si b = a + 2 y tanto a como b son primos. Implementar
	primosGem :: Int -> Int que dado n, devuelve la cantidad
	de pares de primos gemelos (a,b) que verifican b <= n.
	Por ejemplo: primosGem 5 = 1 (porque 3 y 5 es un par de
	primos gemelos) primosGem 14 = 3 (porque 3 y 5, 5 y 7 y 11
	y 13 son tres pares de primos gemelos)
-}
primosGem :: Int -> Int
primosGem n = primosGem_aux n 3 0 

primosGem_aux :: Int -> Int -> Int -> Int 
primosGem_aux n j c |n <= 4 = 0 
                    |n >= p j && esPrimo i = primosGem_aux n (j+1) (c+1)
                    |esPrimo i == False = primosGem_aux n (j+1) (c)
                    |otherwise = c 
	where  
	 	i = p j - 2

{-Ejercicio 16: Conjetura de los primos gemelos
	Existen infinitos pares de primos gemelos. Implementar la 
	función proxPrimosGem :: Int -> (Int,Int) que dado n
	devuelve el primer par de gemelos (a,b) tal que a > n
-}
proxPrimosGem :: Int -> (Int, Int)
proxPrimosGem n = proxPrimosGem_aux n 1 

proxPrimosGem_aux :: Int -> Int -> (Int,Int)
proxPrimosGem_aux n i |n <= 3 = (3,5) 
                      |p i > n && esPrimo j = (p i , j)
                      |otherwise = proxPrimosGem_aux n (i+1)
	where
		j = p i + 2  

{-Ejercico 17: Conjetura de Lothar Collatz
	a) Implementar largoSecuencia :: Int -> Int que dado un
	n > 0 devuelve la cantidad de reducciones desde a1 = n
	hasta llegar a 1. Por ejemplo, largoSecuencia 13 es 9.
-}
largoSecuencia :: Int -> Int 
largoSecuencia n = collatz_a n 0 

collatz_a :: Int -> Int -> Int
collatz_a n c | n < 1 = 0
              | n == 1 = c 
              | esPar n == True = collatz_a (div n 2 ) (c + 1)
              | esPar n == False = collatz_a (3*n + 1) (c + 1)

esPar :: Int -> Bool
esPar n | mod n 2 == 0 = True 
        | otherwise = False 

{-	
	b)Resolver usando Haskell: ¿Qué número menor a 10.000 para
	a1 produce la secuencia más larga hasta llegar a 1? 
	Sugerencia: usar la idea de la función del ejercicio 11. 
-}
masLargo :: Int -> Int 
masLargo an = collatz_b an 1 1 1 

collatz_b :: Int -> Int -> Int -> Int -> Int 
collatz_b an a1 i m | a1 == an - 1 = m 
                    | largoSecuencia m > largoSecuencia (i) = collatz_b an (a1+1) (i+1) m
                    | largoSecuencia m < largoSecuencia (i) = collatz_b an (a1+1) (i+1) (i)
                    | largoSecuencia m == largoSecuencia (i)= collatz_b an (a1+1) (i+1) m 

p :: Int -> Int 
p 1 = 2
p n = minimoPrimoDesde (1 + p (n-1))