module Naturales
where
    import Prelude(Int,Bool(True,False),succ,pred,(||),(&&),not,otherwise)

    suma :: Int -> Int -> Int
    suma 0 n = n
    suma m n = suma (pred m) (succ n)
    
    mult :: Int -> Int -> Int
    mult 0 n = 0
    mult m n = suma n (mult (pred m) n)
     
    resta :: Int -> Int -> Int
    resta 0 _ = 0 
    resta m n = resta (pred m) (pred n)
    
    menor :: Int -> Int -> Bool 
    menor 0 0 = False
    menor _ 0 = False
    menor 0 _ = True
    menor m n = menor (pred m) (pred n)

    mayor :: Int -> Int -> Bool
    mayor 0 0 = False
    mayor 0 _ = False
    mayor _ 0 = True
    mayor m n = mayor (pred m) (pred n)

    iguales :: Int -> Int -> Bool
    iguales 0 0 = True
    iguales _ 0 = False
    iguales 0 _ = False
    iguales m n = iguales (pred m) (pred n)

    max :: Int -> Int -> Int
    max m n |mayor m n = m 
            |otherwise = n  

    min :: Int -> Int -> Int
    min m n |menor m n = m 
            |otherwise = n