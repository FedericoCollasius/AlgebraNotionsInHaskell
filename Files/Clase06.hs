--Teoría--

sumatoriaLista :: [Int] -> Int
sumatoriaLista l | l == [] = 0
                 | otherwise = head l + sumatorialista (tail l)

longitud :: [Int] -> Int
longitud l | l == [] = 0
           | otherwise = 1 + longitud (tail l)

pertenece :: Int -> [Int] -> Bool
pertenece x l | l == [] = False
              | otherwise = (x == head l) || pertenece x (tail l)

primerMultiplode45345 :: [Int] -> Int
primerMultiplode45345 l | mod (head l) 45345 == 0 = (head l)
                        | otherwise = primerMultiplode45345 (tail l)

sumatoriaPM :: [Int] -> Int
sumatoriaPM [] = 0
sumatoriaPM (x:xs) = x + sumatoriaPM xs

longitudPM :: [a] -> Int
longitudPM [] = 0
longitudPM (_:xs) = 1 + longitudPM xs 

--Ejercicios--

{-Ejercicio 0:
	Repensar la función pertence utilizando pattern matching.
-}
pertenecePM :: Int -> [Int] -> Bool
pertenecePM x [] = False
pertenecePM x xs = x == head xs || pertenecePM x (tail xs)

{-Ejercicio 1: productoria :: [Int] -> Int
	Devuelve la productoria de los elementos.	
-}
productorialista :: [Int] -> Int
productorialista [] = 1 
productorialista l  = head l * productorialista (tail l)

{-Ejercicio 2: sumarN :: Int -> [Int] -> [Int]
	Dado un número N y una lista xs, suma N a cada elemento
	de xs.
-}
sumarN :: Int -> [Int] -> [Int] 
sumarN n xs | tail xs == [] = [n + head xs]
            | otherwise     =  head xs + n : sumarN n (tail (xs))

{-Ejercicio 3: sumarElPrimero :: [Int] -> [Int]
	Dado una lista no vacía xs, suma el primer elemento
	elemento de xs. 
-}
sumarElPrimero :: [Int] -> [Int]
sumarElPrimero xs = sumarN (head xs) xs

{-Ejercicio 4: sumarElUltimo :: [Int] -> [Int]
	Dada una lista no vacía xs, suma el último elemento a cada
	elemento de xs.
-}
sumarElUltimo :: [Int] -> [Int]
sumarElUltimo xs = sumarN (head (ultimoElem xs)) xs 

ultimoElem :: [Int] -> [Int]
ultimoElem xs | tail xs == [] = [head xs] 
              | otherwise     = ultimoElem (tail xs) 

{-Ejercicio 5: pares :: [Int] -> [Int] 
	Devuelve una lista con los elementos pares de la lista original.
-}
pares :: [Int] -> [Int]
pares xs | xs == []        = xs
         | esPar (head xs) = head xs : pares (tail xs)
         | otherwise       = pares (tail xs) 

esPar :: Int -> Bool
esPar 0 = True
esPar n = not (esPar (n-1))

{-Ejercicio 6: quitar :: Int -> [Int] -> [Int]
	Elimina la primera aparición del elemento en la lista (de haberla).
-}
quitarPrimera :: Int -> [Int] -> [Int]
quitarPrimera n xs | xs == []     = []
                   | head xs == n = tail xs
                   | otherwise    = head xs : quitarPrimera n (tail xs)

{-Ejercicio 7: quitarTodas :: Int -> [Int] -> [Int]
	Elimina todas las apariciones del elemento en la lista (de haberla).
-}
quitarTodas :: Int -> [Int] -> [Int]
quitarTodas n xs | xs == []      = []
                 | head xs /= n  = head xs : quitarTodas n (tail xs)
                 | otherwise     = quitarTodas n (tail xs)

{-Ejercicio 8: hayRepetidos :: [Int] -> Bool
	Indica si una lista tiene elementos repetidos.
-}

hayRepetidos :: [Int] -> Bool
hayRepetidos xs | tail xs == []                 = False  
                | apareceRepetidoN (head xs) xs = True 
                | otherwise                     = hayRepetidos (tail xs) 

apareceRepetidoN :: Int -> [Int] -> Bool
apareceRepetidoN n xs | tail xs == []       = False 
                      | n == head (tail xs) = True
                      | otherwise           = apareceRepetidoN n (tail xs)

{-Ejercicio 9: eliminarRepetidosAlFinal :: [Int] -> [Int]
	Deja en la lista la primera aparición de cada elemento,
	eliminando las repeticiones adicionales. 
-}
eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal xs | not (hayRepetidos xs)                  = xs
                            | cuantasVecesAparece (head xs) xs  > 1  = eliminarRepetidosAlFinal (dejarPrimera (head xs) (xs) 0)
                            | otherwise                              = head xs : eliminarRepetidosAlFinal (dejarPrimera (head (tail xs)) (tail xs) 0)
                            

dejarPrimera :: Int -> [Int] -> Int -> [Int]
dejarPrimera n xs c | xs == []               = xs
                    | head xs == n && c == 0 = head xs : dejarPrimera n (tail xs) (c + 1)
                    | head xs == n && c >= 1 = dejarPrimera n (tail xs) c 
                    | otherwise              = head xs : dejarPrimera n (tail xs) c 
              
cuantasVecesAparece :: Int -> [Int] -> Int
cuantasVecesAparece n []                 = 0  
cuantasVecesAparece n (x:xs) | n == x    = 1 + cuantasVecesAparece n xs
                             | otherwise = cuantasVecesAparece n xs
 
 {-Ejercicio 10: eliminarRepetidosAlInicio :: [Int] -> [Int] 
	Deja en la lista la última aparición de cada elemento,
	eliminando las repeticiones adicionales.
 -}
eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio xs | not (hayRepetidos xs)                = xs
                             | cuantasVecesAparece (head xs) xs > 1 = eliminarRepetidosAlInicio (quitarPrimera (head xs) xs)
                             | otherwise                            = (head xs) : eliminarRepetidosAlInicio (tail xs)
                              
{-Ejercicio 11: maximo :: [Int] -> Int
	Calcula el máximo elemento de una lista no vacía.
 -}
maximo :: [Int] -> Int 
maximo (s:xs)  | xs == []     = s
               | s > head xs  = maximo (s : tail(xs))
               | otherwise    = maximo xs

{-Ejercicio 12: ordenar :: [Int] -> [Int]
	Ordena los elementos de forma creciente.
-}
ordenar :: [Int] -> [Int]
ordenar xs | tail xs == [] = xs
           | otherwise     = minimo xs : ordenar (quitarPrimera (minimo xs) xs )

minimo :: [Int] -> Int
minimo xs | tail xs == []            = head xs
          | head xs < head (tail xs) = minimo (head xs : tail (tail xs))
          | otherwise                = minimo (tail xs)

{-Ejercico 13: reverso :: [Int] -> [Int]
	Dada una lista invierte su orden.
-}
reverso :: [Int] -> [Int]
reverso xs | tail xs == [] = xs 
           | otherwise     = head (ultimoElem xs) : reverso (quitarUltima  (head (ultimoElem xs)) xs) 

quitarUltima :: Int -> [Int] -> [Int]
quitarUltima n xs | n == head xs && not (apareceRepetidoN n xs) = tail xs 
                  | otherwise                                   = head xs : quitarUltima n (tail xs)

{-Ejercicio 14: concatenar :: [Int] -> [Int] -> [Int]
	Devuelve la concatenación de la primera lista con la segunda.
-}
concatenar :: [Int] -> [Int] -> [Int]
concatenar xs xl | xl == []  = xs
                 | otherwise = concatenar (agregarAlFinal (head xl) xs ) (tail xl) 

agregarAlFinal :: Int -> [Int] -> [Int]
agregarAlFinal n xs | tail xs == [] = head xs : [n]
                    | otherwise     = head xs : agregarAlFinal n (tail xs)

{-Ejercicio 15: zipi :: [a] -> [b] -> [(a,b)]
	Devuelve una lista de tuplas, cada tupla contiene elementos 
	de ambas listas que ocurren en la misma posición. En caso
	que tengan distintas longitudes, la longitud de la lista 
	es igual a la longitud de la lista más chica pasada por
	parámetro.
-}
zipi :: [a] -> [b] -> [(a,b)]     
zipi [] _ = [] 
zipi _ [] = []
zipi (x:xs) (y:ys) = (x,y) : (zipi xs ys)