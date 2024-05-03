module Simulacro1 where

-- aux

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:xs) | x == y = True
                   | otherwise = pertenece x xs

reversoT :: (t,t) -> (t,t)
reversoT (a,b) = (b,a)

longitud :: [t] -> Int
longitud [] = 0
longitud xs = 1 + longitud (tail xs)



-- 1

relacionesValidas :: [(String,String)] -> Bool
relacionesValidas [] = True
relacionesValidas (x:xs) | fst x == snd x = False
                         | xs == [] = True
                         | pertenece x xs || pertenece (reversoT x) xs = False
                         | otherwise = relacionesValidas xs


-- 2 

personas :: [(String,String)] -> [String]
personas [] = []
personas (x:xs) | pertenece (fst x) (personas xs) && pertenece (snd x) (personas xs) = personas xs
                | pertenece (fst x) (personas xs) = (snd x):(personas xs)
                | pertenece (snd x) (personas xs) = (fst x):(personas xs)
                | otherwise = (fst x):(snd x):(personas xs)


-- 3

amigosDe :: String -> [(String,String)] -> [String]
amigosDe _ [] = []
amigosDe p (x:xs) | p == (fst x) = (snd x):(amigosDe p xs)
                  | p == (snd x) = (fst x):(amigosDe p xs)
                  | otherwise = amigosDe p xs


-- 4

personaConMasAmigos :: [(String,String)] -> String
personaConMasAmigos la = masAmigosAux la (personas la)

masAmigosAux :: [(String,String)] -> [String] -> String
masAmigosAux _ [] = " "
masAmigosAux _ [x] = x
masAmigosAux la (p1:p2:ps) | longitud (amigosDe p1 la) >= longitud (amigosDe p2 la) = masAmigosAux la (p1:ps)
                           | otherwise = masAmigosAux la (p2:ps)





