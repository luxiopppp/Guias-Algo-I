-- 1 Definir las siguientes funciones sobre listas

-- 1.1 longitud :: [t] -> Integer, que dada una lista devuelve su cantidad de elementos.

longitud :: [t] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs


-- 1.2 ultimo :: [t] -> t según la especificacián

ultimo :: [t] -> t
ultimo [t] = t
ultimo xs = ultimo (tail xs) -- op 1
-- ultimo (_:xs) = ultimo xs -- op 2


-- 1.3 principio :: [t] -> [t] según la especificación

-- principio :: [t] -> [t]
-- principio 