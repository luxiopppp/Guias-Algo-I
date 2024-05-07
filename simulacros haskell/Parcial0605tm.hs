module Parcial0605 where

-- 1

hayQueCodificar :: Char -> [(Char,Char)] -> Bool
hayQueCodificar _ [] = False
hayQueCodificar c (m:map) 
    | c == fst m = True
    | otherwise = hayQueCodificar c map

-- 2

cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char,Char)] -> Int
cuantasVecesHayQueCodificar _ [] _ = 0
cuantasVecesHayQueCodificar c f map 
    | c == head f && hayQueCodificar c map = 1 + cuantasVecesHayQueCodificar c (tail f) map
    | otherwise = cuantasVecesHayQueCodificar c (tail f) map 

-- 3

laQueMasHayQueCodificar :: [Char] -> [(Char,Char)] -> Char
laQueMasHayQueCodificar f map = laQueMasAux f ('!', 0) map

laQueMasAux :: [Char] -> (Char,Int) -> [(Char,Char)] -> Char
laQueMasAux [] max _ = fst max
laQueMasAux f max map
    | hayQueCodificar (head f) map && cuantasVecesHayQueCodificar (head f) f [(head f,' ')] > snd max = laQueMasAux (tail f) (head f, cuantasVecesHayQueCodificar (head f) f [(head f, ' ')]) map
    | otherwise = laQueMasAux (tail f) max map
    

-- 4

codificarFrase :: [Char] -> [(Char,Char)] -> [Char]
codificarFrase [] _ = []
codificarFrase f map 
    | hayQueCodificar (head f) map = (codificar (head f) map):(codificarFrase (tail f) map)
    | otherwise = (head f):(codificarFrase (tail f) map)

codificar :: Char -> [(Char,Char)] -> Char
codificar l map 
    | l == fst (head map) = snd (head map)
    | otherwise = codificar l (tail map)
