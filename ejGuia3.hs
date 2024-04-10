import Distribution.Simple.Utils (xargs)
-- 1 a,b,c

f,g,h,k :: Int -> Int
f n | n == 1 = 8
    | n == 4 = 131
    | n == 16 = 16

g n | n == 8 = 16
    | n == 16 = 4
    | n == 131 = 1

h n = g (f n)

k n = f (g n)

-- 2 a,c,f,g,i,j

absoluto :: Int -> Int
absoluto x | x < 0 = -x
           | otherwise = x

absolutoF :: Float -> Float
absolutoF x | x < 0 = -x
           | otherwise = x

maximoabsoluto :: Int -> Int -> Int
maximoabsoluto a b | a >= 0 && b <= 0 = max a (-b)
                   | a <= 0 && b >= 0 = max (-a) b
                   | a <= 0 && b <= 0 = max (-a) (-b)
                   | otherwise = max a b

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x > y && x > z = x
              | y > z = y
              | otherwise = z

-- maximo3 x y z | x > y && x > z = x
--               | y > x && y > z = y
--               | z > x && z > y = z

-- maximo3 x y z = max ( x max ( y z ) )

mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo a b  | a <= 3 && b <= 3 = True
                    | a > 3 && a <= 7 && b > 3 && b <= 7 = True
                    | a > 7 && b > 7 = True
                    | otherwise = False


sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z | x /= y && x /= z && y /= z = x + y + z
                    | x /= y && x == z = x + y
                    | x == y && x /= z = x + z
                    | y == z && y /= x = x + y
                    | otherwise = 0

digitoUnidades, digitoDecenas :: Int -> Int
digitoUnidades n = n `mod` 10
digitoDecenas n = n `mod` 100 `div` 10

-- 3

estanRelacionados :: Int -> Int -> Bool
estanRelacionados a b = -(a*a) `div` (a*b) /= 0

-- estanRelacionados a b = mod (a*a) (a*b) == 0



-- 4 a,b,c,e 

prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (x, y) (a, b) = (x * a) + (y * b)

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (x, y) (n, m) = x < n && y < m

distanciaPuntos :: (Int, Int) -> (Int, Int) -> Int
distanciaPuntos (x, y) (n, m) = (n - x) + (m - y)

sumarSoloMultiplos :: (Int, Int, Int) -> Int -> Int
sumarSoloMultiplos (x, y, z) n | mod x n == 0 && mod y n == 0 && mod z n == 0 = x + y + z
                               | mod x n == 0 && mod y n == 0 && mod z n /= 0 = x + y
                               | mod x n == 0 && mod y n /= 0 && mod z n == 0 = x + z
                               | mod x n /= 0 && mod y n == 0 && mod z n == 0 = y + z
                               | otherwise = 0

posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (x, y, z) | mod x 2 == 0 = 1
                       | mod y 2 == 0 = 2
                       | mod z 2 == 0 = 3
                       | otherwise = 4

crearPar :: t -> r -> (t,r)
crearPar a b = (a,b)
