--Teoría-- 

bact :: Int -> Int
bact n | n <= 1 = 1
       | otherwise = bact (n-1) + bact (n-2)
       
fibo = bact       

ida :: Int -> Int
ida 0 = 0
ida n = 1 + ida (n-1)

idb :: Int -> Int
idb 0 = 0
idb n = (-1) + idb (n+1)
esPar1 :: Int -> Bool
esPar1 0 = True
esPar1 1 = False
esPar1 n = esPar1 (n-2)

esPar2 :: Int -> Bool
esPar2 0 = True
esPar2 1 = False
esPar2 n = esPar2 (n+2) && esPar2 (n-2)

unoAdelanteTresAtras :: Int -> Int
unoAdelanteTresAtras 0 = 0
unoAdelanteTresAtras 1 = 1
unoAdelanteTresAtras n | esPar n = 1 + unoAdelanteTresAtras (n+1)
                       | otherwise = 1 + unoAdelanteTresAtras (n-3) 

collatz 1 = 1
collatz n | n `mod` 2 == 0 = collatz (n `div` 2)
          | otherwise = collatz (3*n+1)
                       
esPar :: Int -> Bool
esPar n = parAux (abs n)
    where parAux n = n == 0 || not (parAux (n-1))                       
          

puntajeValido :: Int -> Bool
--opcion 1
puntajeValido 0 = True
puntajeValido n = (n >= 7 && puntajeValido(n-7)) || (n >= 11 && puntajeValido(n-11))
--opcion 2: cortocircuito
puntajeValido' :: Int -> Bool
puntajeValido' n = (n==0) || (n>=7 && puntajeValido'(n-7)) || (n>=11 && puntajeValido'(n-11))          

cantDigitos :: Int -> Int
cantDigitos 0 = 0
cantDigitos n = 1 + cantDigitos (n `div` 10)

esBinario :: Int -> Bool
esBinario 0 = True
esBinario n = n `mod` 10 <= 1 && esBinario (n `div` 10)

esParR :: Int -> Bool                   
esParR 0 = True                      
esParR n = esImparR (n-1)

esImparR :: Int -> Bool       
esImparR 0 = False
esImparR n = esParR (n-1)

bact' :: Int -> Int                      
bact' 0 = 1                              
bact' n = bact' (n-1) + repr (n-1)        

repr :: Int -> Int
repr 0 = 0
repr n = bact' (n-1)

dosParams :: Int -> Int -> Int
dosParams 0 _ = 0
dosParams _ 0 = 0
dosParams m n | m `mod` 2 == 0 = n + dosParams (m-2) (n+1)
              | otherwise = n + dosParams (m+2) (n-1)

parametroMcontrola :: Int -> Int -> Int
parametroMcontrola 0 _ = 0
parametroMcontrola m n = n + (parametroMcontrola (m `div` 2) (2*n))              


--Ejercicios-- 

tribonacci :: Int -> Int 
tribonacci n | n <= 2 = n
             | otherwise = tribonacci(n-1) + tribonacci(n-2) + tribonacci(n-3)

mult3 :: Int -> Bool
mult3 0 = True
mult3 1 = False
mult3 2 = False
mult3 n = mult3(n-3) {-también se puede agregar un abs dentro del parentesis de la  
                      linea 9 y entonces sacar la linea 8 -}

diabolico :: Int -> Bool
diabolico 6 = True
diabolico n |mod n 10 == 6 = diabolico (div n 10)
            |otherwise = False


potenciaOtro :: Int -> Int -> Bool 
potenciaOtro 1 m = True
potenciaOtro 0 m = False
potenciaOtro n m |mod n m == 0 = potenciaOtro (div n m) m
                 |otherwise = False

digitosIguales :: Int -> Bool
digitosIguales n = ( n == n`mod`10) ||(digitosIguales(n`div`10) && (div(n`mod`100)10) == (n`mod`10))

--en una sola linea 

digitosIguales' :: Int -> Bool
digitosIguales' n = n < 10 || ultimosDosDigitosIguales && digitosIguales' quitarUltimoDigito
                where 
                     anteUltimo = (n `mod` 100) `div` 10
                     ultimo = n `mod` 10
                     quitarUltimoDigito = n `div` 10
                     ultimosDosDigitosIguales = ultimo == anteUltimo
--más declarativo 

digitosIguales'' :: Int -> Bool
digitosIguales'' n | div n 10 == 0 = True
                   | mod n 10 == mod (div n 10) 10 = digitosIguales'' (div n 10)  
                   | otherwise = False

--favorita 
