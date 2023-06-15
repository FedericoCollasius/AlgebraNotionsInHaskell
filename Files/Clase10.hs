module Clase10
where

--Funciones de Otras Clases:
minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n+1)

esPrimo :: Int -> Bool
esPrimo n = (n > 1) && not (tieneDivisoresDesde n 2)

tieneDivisoresDesde :: Int -> Int -> Bool
tieneDivisoresDesde n k | k == n = False
                        | otherwise = (mod n k == 0) || tieneDivisoresDesde n (k+1)

nEsimoPrimo :: Int -> Int 
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))

mcd :: Int -> Int -> Int 
mcd _ 1 = 1
mcd a b | a `mod` b == 0 = b 
        | otherwise = mcd b (a `mod` b)

emcd :: Int -> Int -> (Int , Int , Int)
emcd a 0 = (a, 1, 2)
emcd a b =  (d, t, s - (div a b)*t)
    where 
         (d,s,t) = emcd b (mod a b)

quePotenciaLoDivide :: Int -> Int -> Int
quePotenciaLoDivide n p | mod n p == 0 = 1 + quePotenciaLoDivide (div n p) p 
                        | otherwise = 0         

ecEquivalente :: (Int, Int, Int) -> (Int, Int, Int)
ecEquivalente (a, b, m) | mod b d /= 0  = undefined
                        | otherwise = (div a d, div b d, div m d)
 where d = mcd a m 

solucionEcConPropAdic :: (Int, Int, Int) -> (Int, Int)
solucionEcConPropAdic (a, b, m) = (mod (s*b) m, m)
 where (d, s, t) = emcd a m 

solucionEc :: (Int, Int, Int) -> (Int, Int)
solucionEc e = solucionEcConPropAdic (ecEquivalente e)


-- 2 --------------------

sistemaSimplifEquiv :: [(Int, Int, Int)] -> [(Int, Int)]
sistemaSimplifEquiv [] = []
sistemaSimplifEquiv (e:es) = (solucionEc e):(sistemaSimplifEquiv es)


-- 3 --------------------

modulos :: [(Int, Int)] -> [Int]
modulos [] = []
modulos ((r, m):es) = m:(modulos es)

mayorModulo :: [(Int, Int)] -> Int
mayorModulo sist = maximum (modulos sist)

cotaParaPrimoMaloDesde :: [(Int, Int)] -> Int -> Int
cotaParaPrimoMaloDesde sist n | nEsimoPrimo (n+1) > (mayorModulo sist) = n
                              | otherwise = cotaParaPrimoMaloDesde sist (n+1)

cotaParaPrimoMalo :: [(Int, Int)] -> Int
cotaParaPrimoMalo sist = cotaParaPrimoMaloDesde sist 1

cantidadMultiplos :: [Int] -> Int -> Int
cantidadMultiplos [] _ = 0
cantidadMultiplos (m:ms) n | mod m (nEsimoPrimo n) == 0 = 1 + cantidadMultiplos ms n
                           | otherwise = cantidadMultiplos ms n 

esPrimoMalo :: [(Int, Int)] -> Int -> Bool
esPrimoMalo sist n = cantidadMultiplos (modulos sist) n >= 2


todosLosPrimosMalosHasta :: [(Int, Int)] -> Int -> [Int]
todosLosPrimosMalosHasta _ 0 = []
todosLosPrimosMalosHasta sist n | esPrimoMalo sist n = (nEsimoPrimo n):(todosLosPrimosMalosHasta sist (n-1))
                                | otherwise = todosLosPrimosMalosHasta sist (n-1)

todosLosPrimosMalos :: [(Int, Int)] -> [Int]
todosLosPrimosMalos [] = []
todosLosPrimosMalos sist = todosLosPrimosMalosHasta sist (cotaParaPrimoMalo sist)


-- 4 --------------------

solucDosEcPotenciasPrimoOrd :: (Int, Int) -> (Int, Int) -> (Int, Int)
solucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2) | mod (r2-r1) m1 == 0 = (r2, m2)
                                              | otherwise = undefined

solucDosEcPotenciasPrimo :: (Int, Int) -> (Int, Int) -> (Int, Int)
solucDosEcPotenciasPrimo (r1, m1) (r2, m2) | m1 <= m2 = solucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2)
                                           | otherwise = solucDosEcPotenciasPrimoOrd (r2, m2) (r1, m1)

solucSistemaPotenciasPrimo :: [(Int, Int)] -> (Int, Int)
solucSistemaPotenciasPrimo [e] = e
solucSistemaPotenciasPrimo (e1:e2:es) = solucSistemaPotenciasPrimo ((solucDosEcPotenciasPrimo e1 e2):es)


-- 5 --------------------

desdoblarSistemaEnFcionPrimo :: [(Int, Int)] -> Int -> ([(Int, Int)], [(Int, Int)])
desdoblarSistemaEnFcionPrimo [] _ = ([], [])
desdoblarSistemaEnFcionPrimo ((r, m):es) p | k == 0 = (pri, (r, m):seg)
                                           | m == p^k = ((r, m):pri, seg)
                                           | otherwise = ((mod r (p^k), p^k):pri, (mod r (div m (p^k)), div m (p^k)):seg)
 where (pri, seg) = desdoblarSistemaEnFcionPrimo es p 
       k = quePotenciaLoDivide m p

sistemaEquivSinPrimosMalosAux :: [(Int, Int)] -> [Int] -> [(Int, Int)]
sistemaEquivSinPrimosMalosAux sist [] = sist
sistemaEquivSinPrimosMalosAux sist (p:ps) = (solucSistemaPotenciasPrimo pri):(sistemaEquivSinPrimosMalosAux seg ps)  
 where (pri, seg) = desdoblarSistemaEnFcionPrimo sist p

sistemaEquivSinPrimosMalos :: [(Int, Int)] -> [(Int, Int)]
sistemaEquivSinPrimosMalos sist = sistemaEquivSinPrimosMalosAux sist (todosLosPrimosMalos sist)


-- 6 -------------------

solucSistemaModCoprimos :: [(Int, Int)] -> (Int, Int)
solucSistemaModCoprimos [e] = e
solucSistemaModCoprimos ((r1, m1):(r2, m2):es) = solucSistemaModCoprimos ((r, m1*m2):es)
 where (d, s, t) = emcd m1 m2
       r = mod (r1*t*m2 + r2*s*m1) (m1*m2)

solucSistema :: [(Int, Int, Int)] -> (Int, Int)
solucSistema sist = solucSistemaModCoprimos ( sistemaEquivSinPrimosMalos ( sistemaSimplifEquiv sist) )

--Ejercicios--

{-1
Dado un sistema general, decide si cada una de sus ecuaciones vista independientemente de las otras, tiene solución.
-}
cadaEcTieneSoluc :: [(Int, Int, Int)] -> Bool 
cadaEcTieneSoluc [] = True 
cadaEcTieneSoluc (e:sist) = ecEquivalente' e && cadaEcTieneSoluc (sist)

ecEquivalente' :: (Int, Int, Int) -> Bool
ecEquivalente' (a, b, m) = not (mod b d /= 0)  
     where d = mcd a m 

{-2
Dado un sistema simplificado, decide si tiene solución.
-}
tieneSolucionSimplif :: [(Int, Int)] -> Bool 
tieneSolucionSimplif [] = True 
tieneSolucionSimplif sist = sistemaEquivSinPrimosMalos' sist

solucDosEcPotenciasPrimoOrd' :: (Int, Int) -> (Int, Int) -> Bool
solucDosEcPotenciasPrimoOrd' (r1, m1) (r2, m2) = mod (r2-r1) m1 == 0 
                                              

solucDosEcPotenciasPrimo' :: (Int, Int) -> (Int, Int) -> Bool
solucDosEcPotenciasPrimo' (r1, m1) (r2, m2) | m1 <= m2 = solucDosEcPotenciasPrimoOrd' (r1, m1) (r2, m2)
                                            | otherwise = solucDosEcPotenciasPrimoOrd' (r2, m2) (r1, m1)

solucSistemaPotenciasPrimo' :: [(Int, Int)] -> Bool
solucSistemaPotenciasPrimo' [] = True
solucSistemaPotenciasPrimo' (e1:e2:es) = solucDosEcPotenciasPrimo' e1 e2 && solucSistemaPotenciasPrimo' es 



desdoblarSistemaEnFcionPrimo' :: [(Int, Int)] -> Int -> ([(Int, Int)], [(Int, Int)])
desdoblarSistemaEnFcionPrimo' [] _ = ([], [])
desdoblarSistemaEnFcionPrimo' ((r, m):es) p | k == 0 = (pri, (r, m):seg)
                                            | m == p^k = ((r, m):pri, seg)
                                            | otherwise = ((mod r (p^k), p^k):pri, (mod r (div m (p^k)), div m (p^k)):seg)
 where (pri, seg) = desdoblarSistemaEnFcionPrimo' es p 
       k = quePotenciaLoDivide m p

sistemaEquivSinPrimosMalosAux' :: [(Int, Int)] -> [Int] -> Bool
sistemaEquivSinPrimosMalosAux' sist [] = True
sistemaEquivSinPrimosMalosAux' sist (p:ps) = (solucSistemaPotenciasPrimo' pri) && (sistemaEquivSinPrimosMalosAux' seg ps)  
 where (pri, seg) = desdoblarSistemaEnFcionPrimo' sist p

sistemaEquivSinPrimosMalos' :: [(Int, Int)] -> Bool
sistemaEquivSinPrimosMalos' sist = sistemaEquivSinPrimosMalosAux' sist (todosLosPrimosMalos sist)



{-3
Dado un sistema general, decide si tiene solución.
-}
tieneSolucion :: [(Int, Int, Int)] -> Bool
tieneSolucion sist = cadaEcTieneSoluc sist && tieneSolucionSimplif (sistemaSimplifEquiv sist)

{-4
Dados dos números coprimos r y m con 1 <= r < m, encuentra un número primo en la clase de
congruencia X "cong" r (mod m).
-}

sonCoprimos :: Int -> Int -> Bool
sonCoprimos a b = noExisteDivisorComunDesde a b 2 

noExisteDivisorComunDesde :: Int -> Int -> Int -> Bool
noExisteDivisorComunDesde a b n |n > a || n > b = True
                                | otherwise     = not (mod a n == 0 && mod b n == 0) && noExisteDivisorComunDesde a b (n+1)


dirichletDesde :: Int -> Int -> Int -> Int
dirichletDesde r m i |not (sonCoprimos r m) || r < 1 = undefined
                     | esPrimo i = i
                     | otherwise = dirichletDesde r m (i+m)

dirichlet :: Int -> Int -> Int
dirichlet r m = dirichletDesde r m r  