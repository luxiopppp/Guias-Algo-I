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
hayRepetidos (x:xs) | pertenece x xs = True -- se fija si el el head xs aparece en tail xs, sino pasa
                    | otherwise = hayRepetidos xs

