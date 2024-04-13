-- 1 Implementar la función fibonacci:: Integer -> Integer que devuelve el i-ésimo número de Fibonacci.

fibonacci:: Integer ->Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)


-- 2 Implementar una función parteEntera :: Float -> Integer

parteEntera :: Float -> Integer
parteEntera n = floor n


-- 3 Especificar e implementar la función esDivisible :: Integer -> Integer -> Bool que dados dos números naturales determinar si el primero es divisible por el segundo. 

esDivisible :: Integer -> Integer -> Bool
esDivisible 0 _ = True
esDivisible a b | a < b = False
                | otherwise = esDivisible (a - b) b


-- 4 Especificar e implementar la función sumaImpares :: Integer -> Integer que dado n ∈ N sume los primeros n números impares

sumaImpares :: Integer ->Integer
sumaImpares 1 = 1
sumaImpares 0 = 0
sumaImpares n | mod n 2 == 0 = sumaImpares (n - 1)
              | n < 0 = 0
              | otherwise = n + sumaImpares (n - 2)


-- 5 Implementar la función medioFact :: Integer -> Integer que dado n ∈ N calcula n!! = n (n−2)(n−4)· · · .

medioFact :: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact n | n < 0 = n
            | otherwise = n * medioFact (n - 2)


-- 6 Especificar e implementar la función sumaDigitos :: Integer -> Integer que calcula la suma de dígitos de un número natural

sumaDigitos :: Integer -> Integer
sumaDigitos 0 = 0
sumaDigitos x = x `mod` 10 + sumaDigitos (x `div` 10)


-- 7 Implementar la función todosDigitosIguales :: Integer -> Bool que determina si todos los dígitos de un número natural son iguales

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales x | x < 10 = True
                      | mod x 10 == mod (div x 10) 10 = todosDigitosIguales (div x 10)
                      | otherwise = False


-- 8 Implementar la función iesimoDigito :: Integer -> Integer -> Integer que dado un n ∈ N≥0 y un i ∈ N menor o igual a la cantidad de dígitos de n, devuelve el i-ésimo dígito de n.

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = mod (div n (10^(cantDigitos n - i))) 10

cantDigitos :: Integer -> Integer
cantDigitos 0 = 1
cantDigitos n | n < 10 = 1
              | otherwise = 1 + cantDigitos (div n 10)


-- 9 Especificar e implementar una función esCapicua :: Integer -> Bool que dado n ∈ N≥0 determina si n es un número capicúa.

-- ? SOLO DICE TRUE SI EL PRIMER Y ÚLTIMO NUMERO SON IGUALES

esCapicua :: Integer -> Bool
esCapicua n | mod n 10 == iesimoDigito n 1 = True
            | otherwise = False


-- 10 Especificar, implementar y dar el tipo de las siguientes funciones (símil Ejercicio 4 Práctica 2 de Algebra 1).

f1 :: Int -> Int
f1 0 = 1
f1 n = (2 ^ n) + f1 (n - 1)

f2 :: Int -> Int -> Int
f2 1 _ = 1
f2 n q = (q^n) + f2 (n - 1) q

f3 :: Int -> Int -> Int
f3 n q = f2 (2*n) q

-- ? f4 :: Int -> Int -> Int
-- ? f4 0 _ = 1
-- ? f4 n q = ((q^(2*n)) + f4 (n - 1) q) - ((q^(n-1)) + f4 (n - 2) q)

f4 :: Int -> Int -> Int
f4 n q = f3 n q - f2 (n-1) q


-- 11 a Especificar e implementar una función eAprox :: Integer -> Float que aproxime el valor del número e a partir de la siguiente sumatoria

eAprox :: Integer -> Float
eAprox 0 = 0
eAprox x = (1 / factorial x) + eAprox (x - 1)

factorial :: Integer -> Float
factorial 0 = 1
factorial n = fromIntegral n * factorial (n - 1)    -- ? fromIntegral


-- 11 b



-- 13 Especificar e implementar la siguiente función

sumatoriaDoble :: Int -> Int -> Int
sumatoriaDoble 0 _ = 0
sumatoriaDoble n m = sumatoriaDoble (n-1) m + sumatoriaInterna n m


sumatoriaInterna :: Int -> Int -> Int
sumatoriaInterna _ 0 = 0
sumatoriaInterna n j = n^j + sumatoriaInterna n (j-1)

