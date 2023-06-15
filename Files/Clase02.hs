
estanRelacionados :: (Ord t, Num t) => t -> t -> Bool  {- Haskel asigna (Ord a1, Ord a2, Num a1, Num a2) => a1 -> a2 -> Bool porque para comparar los valores con constantes numéricas 
                                                           requiere de Num. Ord permite que se efectuen comparaciones entre valores enteros y flotantes. 
                                                           Haskell infiere que la salida va a ser Bool porque ve que solo hay Trues y Falses -}
{-|
Dados dos números reales, 'estanRelacionados' decide si están relacionados considerando la 
relación de equivalencia en R cuyas clases de equivalencia son:(−Inf,3],(3,7] y (7,Inf).
|-}                                                        
estanRelacionados x y | x <= 3 && y <= 3 = True
                      | (x > 3 && x <= 7) && (y > 3 && y <= 7) = True
                      | x > 7 && y > 7 = True
                      | otherwise = False

prodInt :: (Num t) => (t, t) -> (t, t) -> t {- Haskell asigna  Num t => (t, t) -> (t, t) -> t porque requiere multiplicar a los valores entre ellos por lo tanto tienen que ser 
											   números.  -}
{-| 
'prodInt' calcula el producto interno entre dos vectores de R2. 
|-}
prodInt (x1, y1) (x2, y2) = (x1*x2 + y1*y2)

todoMenor :: (Ord t, Num t) => (t, t) -> (t, t) -> Bool {- Haskell asigna (Ord t, Num t) => (t, t) -> (t, t) -> Bool porque la función exige comparar números (Num), y que se puede 
															comparar entre valores flotantes y numericos. Haskel interpreta que la salida va a ser Bool porque ve solo que hay Trues
															y Flases. -}
{-|
dados dos vectores de R2, 'todoMenor' decide si es cierto que 
cada coordenada del primervector es menor a la coordenada correspondiente del segundo vector.
|-}
todoMenor (x1, y1) (x2, y2) | x1 < x2 && y1 < y2 = True
                            | otherwise = False

distanciaPuntos :: (Floating t, Num t) => (t, t) -> (t, t) -> t {- Floating t => (t, t) -> (t, t) -> t se asigna este valor porque la funcion requiere hacer la raiz cuadrada de un número
                                                                     y esta funcion requiere que sean valores flotantes. Creo que sqrt interpreta a los valores integros como flotantes y por
                                                                     eso funciona sin especificar el Int. -}
{-|
'distanciaPuntos'  calcula la distancia entre dos puntos de R2.
|-}
distanciaPuntos (x1, y1) (x2, y2) = sqrt ((x2-x1)**2 + (y2-y1)**2)

sumaTerna :: (Integral t, Num t) => (t, t, t) -> t {- Num a => (a, a, a) -> a Haskell asigna este valor porque la funcion no acota a valores enteros o flotantes. Así que se asigna uno que funcione
                                                         con todos. -}
{-|
dada una terna de enteros, 'sumaTerna' calcula la suma de sus tres elementos.
|-}
sumaTerna (x, y, z) = x + y + z

posicPrimerPar :: (Integral t, Eq t) => (t, t, t) -> t {- Integral p => (p, p, p) -> p Haskel asigna este valor ya que la funcion mod requiere que sean valores enteros y como si o si son valores enteros 
                                                           los puede comparar con una constante numérica. -}
{-|
dada una terna de enteros, 'posicPrimerPar' devuelve la posición del primer número par si es que hay alguno, 
y devuelve 4 si son todos impares.
|-}
posicPrimerPar (x, y, z) | mod x 2 == 0 = x
                         | mod y 2 == 0 = y
                         | mod z 2 == 0 = z
                         | otherwise = 4

crearPar :: (Eq a, Eq b) => a -> b -> (a, b) {- (Eq a, Eq b) => a -> b -> (a, b) Haskell asigna esta clase de tipo porque entonces da lugar a que escriba cualquier tipo de valor (booleano, integro
                                                  flotante). -}
{-|
'crearPar' crea un par a partir de sus dos componentes dadas por separado.
|-}
crearPar x y = (x, y)

invertir :: (Ord a, Ord b) => (a,b) -> (b,a) {- (b, a) -> (a, b) Haskell no asigna ninguna clase de tipo porque  -}
{-|
'invertir'  invierte los elementos del par pasado como parámetro.
|-}
invertir (x, y) = (y, x)




                      
