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



-- 4 a,b,c,d,e,f,g,h

prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (x, y) (a, b) = (x * a) + (y * b)

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (x, y) (n, m) = x < n && y < m

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x, y) (n, m) = ( (n - x) ** 2 + (m - y) ** 2) ** 0.5
-- distanciaPuntos (x, y) (n, m) = sqrt ( (n - x) ** 2 + (m - y) ** 2)

sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x,y,z) = x + y + z

sumarSoloMultiplos :: (Int, Int, Int) -> Int -> Int
sumarSoloMultiplos (x, y, z) n | mod x n == 0 && mod y n == 0 && mod z n == 0 = x + y + z
                               | mod x n == 0 && mod y n == 0 && mod z n /= 0 = x + y
                               | mod x n == 0 && mod y n /= 0 && mod z n == 0 = x + z
                               | mod x n /= 0 && mod y n == 0 && mod z n == 0 = y + z
                               | mod x n == 0 = x
                               | mod y n == 0 = y
                               | mod z n == 0 = z
                               | otherwise = 0

posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (x, y, z) | mod x 2 == 0 = 1
                       | mod y 2 == 0 = 2
                       | mod z 2 == 0 = 3
                       | otherwise = 4

crearPar :: t -> r -> (t,r)
crearPar a b = (a,b)

invertir :: (a, b) -> (b, a)
invertir (a,b) = (b,a)


-- 5

todosMenores :: (Int, Int, Int) -> Bool
todosMenores (x, y, z) = u x > l x && u y > l y && u z > l z

u :: Int -> Int
u n | n <= 7 = n * n
    | n > 7 = ( 2 * n ) - 1

l :: Int -> Int
l n | mod n 2 == 0 = div n 2
    | mod n 2 /= 0 = ( 3 * n ) + 1


-- 6

bisiesto :: Int -> Bool
bisiesto n | mod n 4 == 0 && mod n 100 /= 0 = True
           | mod n 400 == 0 = True
           | otherwise = False


-- 7

distanciaManhattan:: (Float, Float, Float) ->(Float, Float, Float) ->Float
distanciaManhattan (x,y,z) (a,b,c) = absolutoF (x - a) + absolutoF (y - b) + absolutoF (z - c)


-- 8

comparar :: Int -> Int -> Int
comparar a b | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
             | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = -1
             | otherwise = 0

sumaUltimosDosDigitos :: Int -> Int -- ⌊ ⌋ "parte entera"
sumaUltimosDosDigitos x | x < 0 = sumaUltimosDosDigitos (-x)
                        | otherwise = (x `mod` 10) + (x `div` 10 `mod` 10)


-- FIN GUIA 3