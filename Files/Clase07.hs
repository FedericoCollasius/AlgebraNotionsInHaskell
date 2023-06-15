module Clase07 where 
type Set a = [a]

--Teoría--

vacio :: Set Int
vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece _ []                 = False
pertenece x (y:ys) | x == y    = True
                   | otherwise = pertenece x ys

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC xs [] = False
perteneceC xs (ys:yss) = iguales xs ys || perteneceC xs yss

agregar :: Int -> Set Int -> Set Int
agregar x c | pertenece x c = c
            | otherwise    = x:c

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | perteneceC xs xss = xss
                | otherwise = xs : xss

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos x [] = []
agregarATodos x (c:cs) = agregarC (agregar x c) (agregarATodos x cs)

incluido :: Set Int -> Set Int -> Bool
incluido [] c = True 
incluido (x:xs) c = pertenece x c && incluido xs c 

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1 

cardinal :: Set Int -> Int 
cardinal c = longitudPM c 

longitudPM :: [a] -> Int
longitudPM [] = 0
longitudPM (_:xs) = 1 + longitudPM xs 

unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC (xs:xss) [] = (xs:xss)
unionC [] (ys:yss) = (ys:yss)
unionC (xs:xss) (ys:yss) = unionC (xss) (agregarC xs (ys:yss))

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = unionC (partes xs) (agregarATodos x (partes xs)) 


--Ejercicios-- 

{-Ejercicio 1: union :: Set Int -> Set Int -> Set Int
	Dado dos conjuntos, devuelve la unión entre ellos.
-}
union :: Set Int -> Set Int -> Set Int 
union (x:xs) [] = (x:xs)
union [] (y:ys) = (y:ys)
union (x:xs) (y:ys) = union (xs) (agregar x (y:ys)) 

{-Ejercicio 2: interseccion :: Set Int -> Set Int -> Set Int 
	Dado dos conjuntos, devuelve la intersección entre ellos. 
-}
interseccion :: Set Int -> Set Int -> Set Int
interseccion (_) [] = []
interseccion [] (_) = []
interseccion (x:xs) (y:ys) | pertenece x (y:ys) = x : interseccion (xs) (y:ys)
                           | otherwise = interseccion (xs) (y:ys)

{-Ejercicio 3: diferencia :: Set Int -> Set Int -> Set Int
	Dado los conjuntos A y B, devuelve. A/B	
-}                           
diferencia :: Set Int -> Set Int -> Set Int
diferencia [] (y:ys) = []
diferencia (x:xs) (y:ys) | pertenece x (y:ys) = diferencia (xs) (y:ys)
                         | otherwise = x : diferencia (xs) (y:ys)


{-Ejercicio 4: diferenciaSimetrica :: Set Int -> Set Int -> Set Int
	Dado los conjuntos A y B, devuelve la diferencia simétrica.
-}
diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica (x:xs) (y:ys) = diferencia (union (x:xs) (y:ys)) (interseccion (x:xs) (y:ys)) 


{-Ejercicio 5: partesN :: Int -> Set Int
	Genera los subconjuntos del conjunto {1,2,3,...,n}
-}
partesN :: Int -> Set (Set Int)
partesN 0 = [[]]
partesN n = unionC (partesN (n-1)) (agregarATodos n (partesN (n-1)))

{-Ejercicio 6: productoCartesiano :: Set Int -> Set Int -> Set (Int,Int)
	Dados dos conjuntos genere todos los pares posibles (como pares de dos elementos)
	tomando el primer elemento del primer conjunto y el segundo elemento del segundo
	conjunto. 	
-}
productoCartesiano :: Set Int -> Set Int -> Set (Int,Int)  
productoCartesiano (_) [] = []
productoCartesiano [] (_) = []
productoCartesiano (x:xs) (y:ys) = unionTup (productoCartesiano_aux x (y:ys)) (productoCartesiano xs (y:ys))

productoCartesiano_aux :: Int -> Set Int -> Set (Int,Int)
productoCartesiano_aux x [] = []
productoCartesiano_aux x (y:ys) = unionTup ([(x,y)]) (productoCartesiano_aux x ys)    

unionTup :: Set (Int,Int) -> Set (Int,Int) -> Set (Int,Int) 
unionTup (x:xs) [] = (x:xs)
unionTup  [] (y:ys) = (y:ys)
unionTup (x:xs) (y:ys) = unionTup (xs) (agregarTup x (y:ys))


agregarTup :: (Int,Int) -> Set (Int,Int) -> Set (Int,Int)
agregarTup x c | perteneceTup x c = c
               | otherwise    = x:c 

perteneceTup :: (Int,Int) -> Set (Int,Int) -> Bool
perteneceTup _ []                 = False
perteneceTup x (y:ys) | x == y    = True
                      | otherwise = perteneceTup x ys