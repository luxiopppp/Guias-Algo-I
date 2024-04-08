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

-- 2 c,g,i,j

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x > y && x > z = x
              | y > x && y > z = y
              | z > x && z > y = z

-- maximo3 x y z = max ( x max ( y z ) )

sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z | x /= y && x /= z && y /= z = x + y + z
                    | x /= y && x == z = x + y
                    | x == y && x /= z = x + z
                    | y == z && y /= x = x + y
                    | otherwise = 0

digitoUnidades, digitoDecenas :: Int -> Int
digitoUnidades n = n `mod` 10
digitoDecenas n = n `mod` 100 `div` 10

-- 4 b

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (x, y) (n, m) = x < n && y < m

