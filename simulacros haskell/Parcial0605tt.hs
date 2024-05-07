-- aux

division :: Int -> Int -> Float
division a b = fromIntegral a / fromIntegral b

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y = True | otherwise = pertenece x ys

-- 1

aproboMasDeNMaterias :: [([Char],[Int])] -> [Char] -> Int -> Bool
aproboMasDeNMaterias [] _ _ = False
aproboMasDeNMaterias (l:ls) a n
    | a == fst l && materiasAprobadas (snd l) > n = True
    | otherwise = aproboMasDeNMaterias ls a n

materiasAprobadas :: [Int] -> Int
materiasAprobadas [] = 0
materiasAprobadas [x] | x >= 4 = 1 | otherwise = 0
materiasAprobadas (x:xs) | x >= 4 = 1 + materiasAprobadas xs | otherwise = materiasAprobadas xs

-- 2

buenosAlumnos :: [([Char],[Int])] -> [[Char]]
buenosAlumnos [] = []
buenosAlumnos (a:as) 
    | esBuenAlumno (snd a) = (fst a):(buenosAlumnos as)
    | otherwise = buenosAlumnos as

esBuenAlumno :: [Int] -> Bool
esBuenAlumno xs 
    | materiasAprobadas xs > (longitud xs - 1) && division (sumatoria xs) (longitud xs) >= 8.0 = True
    | otherwise = False

-- 3

mejorPromedio :: [([Char],[Int])] -> [Char]
mejorPromedio as = mejorPromAux as (fst (head as)) 0

mejorPromAux :: [([Char],[Int])] -> [Char] -> Float -> [Char]
mejorPromAux [] mvp _ = mvp
mejorPromAux (a:as) mvp max
    | division (sumatoria (snd a)) (longitud (snd a)) > max = mejorPromAux as (fst a) (division (sumatoria (snd a)) (longitud (snd a)))
    | otherwise = mejorPromAux as mvp max

-- 4

seGraduoConHonores :: [([Char],[Int])] -> Int -> [Char] -> Bool
seGraduoConHonores as mat a
    | aproboMasDeNMaterias as a (mat - 1) && pertenece a (buenosAlumnos as) && promedioNotas a as > (promedioNotas (mejorPromedio as) as - 1) = True
    | otherwise = False

promedioNotas :: [Char] -> [([Char],[Int])] -> Float
promedioNotas a (n:ns) 
    | a == fst n = division (sumatoria (snd n)) (longitud (snd n))
    | otherwise = promedioNotas a ns
