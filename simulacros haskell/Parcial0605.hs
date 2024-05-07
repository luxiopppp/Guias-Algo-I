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
laQueMasHayQueCodificar f _ = laQueMasAux f ('!', 0)

laQueMasAux :: [Char] -> (Char,Int) -> Char
laQueMasAux [] max = fst max
laQueMasAux f max 
    | cuantasVecesHayQueCodificar (head f) f [(head f,' ')] > snd max = laQueMasAux (tail f) (head f, cuantasVecesHayQueCodificar (head f) f [(head f, ' ')])
    | otherwise = laQueMasAux (tail f) max
    

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
