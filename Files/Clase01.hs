absoluto :: Int -> Int
absoluto n | n >= 0 = n
           | otherwise =(-1)*n

maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y | absoluto x > absoluto y = x
                   | otherwise = y

maximo3 :: Int -> Int -> Int -> Int 
maximo3 x y z | x > y && x > z = x
              | y > x && y > z = y
              | z > x && z > y = z

algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y | x == 0 = True
              | y == 0 = True
              | otherwise = False

algunoEs0PM :: Float -> Float -> Bool              
algunoEs0PM _ 0 = True
algunoEs0PM 0 _ = True
algunoEs0PM x y | otherwise = False

ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y | x == 0 && y == 0 = True
              | otherwise = False 

ambosSon0PM :: Float -> Float -> Bool
ambosSon0PM 0 0 = True
ambosSon0PM x y | otherwise = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | x <= 0 = undefined 
                 | y <= 0 = undefined 
                 | mod x y == 0 = True
                 | otherwise = False

digitoUnidades :: Int -> Int
digitoUnidades n | n <= 0 = undefined
                 | otherwise = mod n 10 
                   
digitoDecenas :: Int -> Int
digitoDecenas n  | n <= 0 = undefined
                 | otherwise = div (mod n 100) 10
                 
		        












