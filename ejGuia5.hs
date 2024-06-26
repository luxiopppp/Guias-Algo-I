-- 1 Definir las siguientes funciones sobre listas

-- 1.1 longitud :: [t] -> Integer, que dada una lista devuelve su cantidad de elementos.

longitud :: [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs


-- 1.2 ultimo :: [t] -> t según la especificacián

ultimo :: [t] -> t
-- ultimo [t] = t
-- ultimo xs = ultimo (tail xs) -- op 1 (pattern matching)
ultimo (x:xs) | longitud xs == 0 = x -- op 2 (con guardas)
              | otherwise = ultimo xs 


-- 1.3 principio :: [t] -> [t] según la especificación

principio :: [t] -> [t]
principio [x] = []
principio (x:xs) = x:(principio xs)


-- 1.4 reverso :: [t] -> [t] según la especificación

reverso :: [t] -> [t]
reverso [] = []
reverso xs = (ultimo xs):(reverso (principio xs)) -- op 1
-- reverso (x:xs) = reverso xs ++ [x] -- op 2


-- 2 Definir las siguientes funciones sobre listas

-- 2.1 pertenece :: (Eq t) => t -> [t] -> Bool según la  especificación

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece x xs = perAux x xs 0 -- op 1 (mia)
  where perAux x xs c | c >= longitud xs = False
                      | x == xs !! c = True
                      | otherwise = perAux x xs (c + 1)

-- pertenece _ [] = False -- op 2 (github)
-- pertenece n (x:xs) | n == x = True
--                    | otherwise = pertenece n xs


-- 2.2 todosIguales :: (Eq t) => [t] -> Bool, que dada una lista devuelve verdadero sí y solamente sí todos sus elementos son iguales.

todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:xs) | x /= y = False
                      | otherwise = todosIguales (y:xs)


-- 2.3 todosDistintos :: (Eq t) => [t] -> Bool según la  especificación

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:y:xs) | x == y = False -- ! importante esto y no "x /= y = True" ya que retorna siempre True
                        | otherwise = todosDistintos (y:xs)


-- 2.4 hayRepetidos :: (Eq t) => [t] -> Bool según la especificación

hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos [x] = False
hayRepetidos (x:xs) | pertenece x xs = True -- se fija si el head xs aparece en tail xs, sino pasa
                    | otherwise = hayRepetidos xs


-- 2.5 quitar :: (Eq t) => t -> [t] -> [t], que dados un entero n y una lista xs, elimina la primera aparición de x en la lista xs (de haberla).

quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar n (x:xs) | n == x = xs
                | otherwise = x:(quitar n xs)


-- 2.6 quitarTodos :: (Eq t ) => t -> [t] -> [t], que dados un entero n y una lista xs, elimina todas las apariciones de x en la lista xs (de haberlas).

quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos n (x:xs) | n == x = quitarTodos n xs
                     | otherwise = x:(quitarTodos n xs)


-- prueba quitarNVeces :: (Eq t ) => t -> Integer -> [t] -> [t], que dados dos enteros n y x y una lista xs, elimina el numero n, x cantidad de veces de la lista xs

quitarNVeces :: (Eq t ) => t -> Integer -> [t] -> [t]
quitarNVeces _ _ [] = []
quitarNVeces n v (x:xs) | v == 0 = (x:xs)
                        | n == x = quitarNVeces n (v-1) xs
                        | otherwise = x:(quitarNVeces n v xs)


-- 2.7 eliminarRepetidos :: (Eq t) => [t] -> [t] que deja en la lista una única aparición de cada elemento, eliminando las repeticiones adicionales.

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:y:xs) | x == y && pertenece x xs == True = eliminarRepetidos xs
                           | x == y || pertenece x xs == True = eliminarRepetidos (y:xs)
                           | otherwise = x:y:(eliminarRepetidos xs)


-- 2.8 mismosElementos :: (Eq t) => [t] -> [t] -> Bool, que dadas dos listas devuelve verdadero sı́ y solamente sı́ ambas listas contienen los mismos elementos, sin tener en cuenta repeticiones

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos xs ys = incluido xs ys && incluido ys xs

incluido :: (Eq t) => [t] -> [t] -> Bool
incluido [] _ = True
incluido _ [] = False
incluido xs ys | pertenece (head xs) ys = incluido (tail xs) ys
               | otherwise = False


-- 2.9 capicua :: (Eq t) => [t] -> Bool según la especificación

capicua :: (Eq t) => [t] -> Bool
capicua xs = xs == (reverso xs)


-- 3 Definir las siguientes funciones sobre listas de enteros

-- 3.1 sumatoria :: [Integer] -> Integer según la especificación

sumatoria :: [Integer] -> Integer
sumatoria xs = sumAux xs 0
  where sumAux xs c | xs == [] = 0
                    | otherwise = (head xs) + sumAux (tail xs) c


-- 3.2 productoria :: [Integer] -> Integer según la especificación

productoria :: [Integer] -> Integer
productoria xs = prodAux xs 0
  where prodAux xs c | xs == [] = 1
                     | otherwise = (head xs) * prodAux (tail xs) c


-- 3.3 maximo :: [Integer] -> Integer según la especificación

maximo :: [Integer] -> Integer
maximo [] = 0
maximo [x] = x
maximo (x:y:xs) | x >= y = maximo (x:xs)
                | otherwise = maximo (y:xs)


-- 3.4 sumarN :: Integer -> [Integer] -> [Integer] según la especificación

sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n (x:xs) = (n+x):(sumarN n xs)


-- 3.5 sumarElPrimero :: [Integer] -> [Integer] según la especificación

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero xs = sumarN (head xs) xs


-- 3.6 sumarElUltimo :: [Integer] -> [Integer] según la especificación

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo xs = sumarN (ultimo xs) xs


-- 3.7 pares :: [Integer] -> [Integer] según la especificación

pares :: [Integer] -> [Integer]
pares [] = []
pares xs | mod (head xs) 2 == 0 = (head xs):(pares (tail xs))
         | otherwise = pares (tail xs)


-- 3.8 multiplosDeN :: Integer -> [Integer] -> [Integer] que dado un número n y una lista xs, devuelve una lista con los elementos de xs múltiplos de n.

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n xs | mod (head xs) n == 0 = (head xs):(multiplosDeN n (tail xs))
         | otherwise = multiplosDeN n (tail xs)


-- 3.9 ordenar :: [Integer] -> [Integer] que ordena los elementos de la lista en forma creciente. Sugerencia: Pensar cómo pueden usar la función máximo para que ayude a generar la lista ordenada necesaria.

ordenar :: [Integer] -> [Integer]
ordenar xs = reverso(ordAux xs)
  where ordAux xs | xs == [] = []
                  | otherwise = (maximo xs):(ordAux (quitar (maximo xs) xs))


-- 4.1 Definir las siguientes funciones sobre listas de caracteres, interpretando una palabra como una secuencia de caracteres sin blancos

-- 4a sacarBlancosRepetidos :: [Char] -> [Char], que reemplaza cada subsecuencia de blancos contiguos de la primera lista por un solo blanco en la lista resultado.

sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:y:xs) | x == ' ' && y == x = sacarBlancosRepetidos (y:xs)
                               | otherwise = x:(sacarBlancosRepetidos (y:xs))

-- 4b contarPalabras :: [Char] -> Integer, que dada una lista de caracteres devuelve la cantidad de palabras que tiene.

contarPalabras :: [Char] -> Integer
contarPalabras [] = 0
contarPalabras [x] = 1
contarPalabras xs | (head (tail xs)) == ' ' = 1 + contarPalabras (tail xs)
                  | otherwise = contarPalabras (tail xs)


-- 4c palabras :: [Char] -> [[Char]], que dada una lista arma una nueva lista con las palabras de la lista original.

palabras :: [Char] -> [[Char]]
palabras xs = palabrasAux xs [] []

palabrasAux :: [Char] -> [Char] -> [[Char]] -> [[Char]] -- ! RECONTRA REBUSCADO MAL
palabrasAux (x:xs) cache l | xs == [] && x /= ' ' = reverso ((reverso (x:cache)):l)
                           | xs == [] = reverso ((reverso cache):l)
                           | x /= ' ' = palabrasAux xs (x:cache) l
                           | otherwise = palabrasAux xs [] ((reverso cache):l)

-- palabras :: [Char] -> [[Char]]
-- palabras lista = palabrasAux lista []

-- palabrasAux :: [Char] -> [Char] -> [[Char]]
-- palabrasAux [] actual = [actual]
-- palabrasAux (x:xs) actual | x /= ' ' = (palabrasAux xs (actual ++ [x]))
--                           | x == ' ' = [actual] ++ (palabrasAux xs []) 


-- 4d palabraMasLarga :: [Char] -> [Char], que dada una lista de caracteres devuelve su palabra más larga.

palabraMasLarga :: [Char] -> [Char]
palabraMasLarga xs = masLargaAux xs [] []

masLargaAux :: [Char] -> [Char] -> [Char] -> [Char] -- xs = lista original, m = max, aux
masLargaAux [] m _ = m
masLargaAux (x:xs) m aux | x == ' ' && longitud aux > longitud m = masLargaAux xs aux []
                         | x == ' ' = masLargaAux xs m []
                         | otherwise = masLargaAux xs m (aux ++ [x])


-- 4e aplanar :: [[Char]] -> [Char] , que a partir de una lista de palabras arma una lista de caracteres concatenándolas.

aplanar :: [[Char]] -> [Char]
aplanar [] = " "
aplanar [x] = x
aplanar (x:xs) = x ++ aplanar xs


-- 4f aplanarConBlancos :: [[Char]] -> [Char], que a partir de una lista de palabras, arma una lista de caracteres concatenándolas e insertando un blanco entre cada palabra.

aplanarConBlancos :: [[Char]] -> [Char]
aplanarConBlancos [] = " "
aplanarConBlancos [x] = x
aplanarConBlancos (x:xs) = x ++ " " ++ aplanarConBlancos xs


-- 4g aplanarConNBlancos :: [[Char]] -> Integer -> [Char], que a partir de una lista de palabras y un entero n, arma una lista de caracteres concatenándolas e insertando n blancos entre cada palabra (n debe ser no negativo).

aplanarConNBlancos :: [[Char]] -> Integer -> [Char]
aplanarConNBlancos [] n = " "
aplanarConNBlancos [x] _ = x
aplanarConNBlancos (x:xs) n = x ++ cantBlancos n ++ aplanarConNBlancos xs n

cantBlancos :: Integer -> [Char]
cantBlancos 1 = " "
cantBlancos n = " " ++ cantBlancos (n-1)


-- 5 Definir las siguientes funciones sobre listas

-- 5.1 sumaAcumulada :: (Num t) => [t] -> [t] según la especificación

sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada [] = []
sumaAcumulada [x] = [x]
sumaAcumulada xs = sumaAcumulada (principio xs) ++ [(ultimo xs) + ultimo (sumaAcumulada (principio xs))]


-- 5.2 descomponerEnPrimos :: [Integer] -> [[Integer]] según la especificación

descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [] = []
descomponerEnPrimos xs | esPrimo (head xs) = [head xs]:descomponerEnPrimos (tail xs)
                       | (tail xs) == [] = [descomponerAux (head xs) 1 []]
                       | otherwise = descomponerAux (head xs) 1 []:descomponerEnPrimos (tail xs)

descomponerAux :: Integer -> Integer -> [Integer] -> [Integer] -- x = head x, i = iesimo primo, xs = res
descomponerAux x i xs | esPrimo x = x:xs
                      | mod x (nEsimoPrimo i) == 0 = descomponerAux (div x (nEsimoPrimo i)) 1 ((nEsimoPrimo i):xs)
                      | otherwise = descomponerAux x (i+1) xs






esDivisible :: Integer -> Integer -> Bool
esDivisible 0 _ = True
esDivisible a b | a < b = False
                | otherwise = esDivisible (a - b) b
menorDivisor :: Integer -> Integer
menorDivisor 1 = 1
menorDivisor n = menorDivisorAux n 2 -- 2 porque pide menor divisor mayor que 1

menorDivisorAux :: Integer -> Integer -> Integer
menorDivisorAux n m | esDivisible n m = m -- m <=> mod n m == 0 => si n primo m <=> m = n
                    | otherwise = menorDivisorAux n (m+1)

esPrimo :: Integer -> Bool
esPrimo n = n == menorDivisor n

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo n = nEsimoPrimoAux n 0 2

nEsimoPrimoAux :: Integer -> Integer -> Integer -> Integer
nEsimoPrimoAux n c p | esPrimo p && c == n-1 = p -- CASO BASE
                     | esPrimo p = nEsimoPrimoAux n (c+1) (p+1) -- si p es primo pero el contador no es igual a n pasa
                     | otherwise = nEsimoPrimoAux n c (p+1) -- si p no es primo aumenta p en uno y pasa

