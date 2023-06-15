module Clase08
where
import Clase07(partes,agregarATodos,unionC,agregarC,perteneceC,incluido,iguales,pertenece)
-------------------------------------------------------------------------------
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

type Set a = [a]

vacio :: Set a
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar n c | n `elem` c = c
            | otherwise = n:c

union :: Eq a => Set a -> Set a -> Set a
union [] ys     = ys
union (x:xs) ys = union xs (agregar x ys)

--------------------------------------------------------------------------------
---------------------------- Funciones Clase 8 ---------------------------------
--------------------------------------------------------------------------------

combinatorio :: Int -> Int -> Int 
combinatorio n k = fact n `div` ((fact k) * (fact (n - k )))

combinatorio' :: Int -> Int -> Int 
combinatorio' n 0 = 1
combinatorio' n k | n == k     = 1
                  | otherwise = (combinatorio' (n-1) k) + (combinatorio' (n-1) (k-1))

agregarElementosAdelante :: Int -> Set [Int] -> Set [Int]
agregarElementosAdelante x []       = []
agregarElementosAdelante x (ys:yss) = agregar (x:ys) (agregarElementosAdelante x yss)   

agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementosAListas [] _ = []
agregarElementosAListas (x:xs) c = (agregarElementosAdelante x c) `union` (agregarElementosAListas xs c)

variaciones :: Set Int -> Int -> Set [Int]
variaciones c 0 = [[]]
variaciones c k = agregarElementosAListas c (variaciones c (k-1))

insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn xs n i | i == 1    = n : xs 
                  | otherwise = (head xs) : (insertarEn (tail xs) n (i-1))

insertarEnCadaPos :: [Int] -> Int -> Int -> Set [Int]
insertarEnCadaPos xs c 1 = agregar (insertarEn xs c 1) vacio 
insertarEnCadaPos xs c i = agregar (insertarEn xs c i) (insertarEnCadaPos xs c (i-1)) 

insertarEnCadaPosDeTodasLasListas :: Set [Int] -> Int -> Set [Int]
insertarEnCadaPosDeTodasLasListas [] c = []
insertarEnCadaPosDeTodasLasListas (xs:xss) c = insertarEnCadaPos xs c (length xs + 1) `union` (insertarEnCadaPosDeTodasLasListas xss c) 

permutaciones :: Set Int -> Set [Int]
permutaciones [] = [[]]
permutaciones (c:cs) = insertarEnCadaPosDeTodasLasListas (permutaciones cs) c

--Ejercicios--

{-Ejercicio 1: bolitasEnCajas :: Int -> Int -> Set [Int]
Todas las formas de ubicar n bolitas numeradas en k cajas.
-}
bolitasEnCajas :: Int -> Int -> Set [Int]  
bolitasEnCajas n k = variaciones ([1..k]) n  

{-Ejercicio 2: 
Todas las formas de ubicar n bolitas numeradas en k cajas tal que la primera
caja nunca esté vacía.
-} 
bolitasEnCajasCond :: Int -> Int -> Set [Int]
bolitasEnCajasCond n k = filtrarElemento (variaciones ([1..k]) n) 1 

filtrarElemento :: Set [Int] -> Int -> Set [Int]
filtrarElemento [] e       = []
filtrarElemento (xs:xss) e | estaEenLista xs e = xs : (filtrarElemento (xss) e) 
                           | otherwise = filtrarElemento (xss) e 

estaEenLista :: Set Int -> Int -> Bool
estaEenLista [] e = False 
estaEenLista (x:xs) e | x == e = True
                      | otherwise = estaEenLista xs e

{-Ejercicio 3:
Todas las listas ordenadas de k números distintos tomados del conjunto {1,...,n}
-}
listasOrdenadas :: Int -> Int -> Set [Int]
listasOrdenadas n k = filtrarTam (partes [1..k]) n

filtrarTam :: Set (Set Int) -> Int -> Set (Set Int)
filtrarTam [] n = []
filtrarTam (xs:xss) n | length xs == n = xs : filtrarTam xss n 
                      | otherwise = filtrarTam xss n  

{-Ejercicio 4:
Todas las sucesiones de los caracteres 'a' y 'b' de longitud n y m respectivamente.
-}
insertarEnLetras :: String -> Char -> Int -> String
insertarEnLetras xs n i | i == 1    = n : xs 
                  | otherwise = (head xs) : (insertarEnLetras (tail xs) n (i-1))

insertarEnCadaPosLetras :: String -> Char -> Int -> Set String
insertarEnCadaPosLetras xs c 1 = agregar (insertarEnLetras xs c 1) vacio 
insertarEnCadaPosLetras xs c i = agregar (insertarEnLetras xs c i) (insertarEnCadaPosLetras xs c (i-1)) 

insertarEnCadaPosDeTodasLasListasLetras :: Set String -> Char -> Set String
insertarEnCadaPosDeTodasLasListasLetras [] c = []
insertarEnCadaPosDeTodasLasListasLetras (xs:xss) c = insertarEnCadaPosLetras xs c (length xs + 1) `union` (insertarEnCadaPosDeTodasLasListasLetras xss c) 

permutacionesLetras :: Set String -> Set String
permutacionesLetras [] = [[]]
permutacionesLetras (c:cs) = insertarEnCadaPosDeTodasLasListasLetras (permutacionesLetras cs) (head c) 

anagramasAB :: Int -> Int -> Set String
anagramasAB m n = permutacionesLetras (listaA m `unionAnag` listaB n)

listaA :: Int -> Set String
listaA 0 = []
listaA m = ['a'] : listaA (m-1)

listaB :: Int -> Set String
listaB 0 = []
listaB n = ['b'] : listaB (n-1)

agregarAnag :: Eq a => a -> Set a -> Set a
agregarAnag n c = n:c

unionAnag :: Eq a => Set a -> Set a -> Set a
unionAnag [] ys     = ys
unionAnag (x:xs) ys = unionAnag xs (agregarAnag x ys)

{-Ejercicio 5:
Todas las sucesiones de los caracteres 'a', 'b' y c de longitud n, m y k respectivamente.	
-}
anagramasABC :: Int -> Int -> Int -> Set String 
anagramasABC m n k = permutacionesLetras ((listaA m `unionAnag` listaB n) `unionAnag` listaC k )

listaC :: Int -> Set String
listaC 0 = []
listaC k = ['c'] : listaC (k-1)

{-Ejercicio 6: subconjuntos :: Set Int -> Int -> Set (Set Int)
Dados un conjunto de enteros y un entero k, genera  todos los subconjuntos  de k elementos
del conjunto pasado por parámetro.
-}
subconjuntos :: Set Int -> Int -> Set (Set Int)
subconjuntos c x = listasOrdenadas' c x 

listasOrdenadas' :: Set Int -> Int -> Set [Int]
listasOrdenadas' c x = filtrarTam (partes c ) x