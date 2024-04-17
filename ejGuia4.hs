-- 1 Implementar la función fibonacci:: Integer -> Integer que devuelve el i-ésimo número de Fibonacci.

fibonacci:: Integer ->Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)


-- 2 Implementar una función parteEntera :: Float -> Integer

parteEntera :: Float -> Integer
parteEntera x | x >= 0 && x < 1 = 0
              | x >= 1 = 1 + parteEntera (x-1)
              | otherwise = 1 + parteEntera (x+1)


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
                      | mod x 10 == mod (c x) 10 = todosDigitosIguales (c x)
                      | otherwise = False
                          where c n = div n 10 -- c = Numero Sin Unidades

-- 8 Implementar la función iesimoDigito :: Integer -> Integer -> Integer que dado un n ∈ N≥0 y un i ∈ N menor o igual a la cantidad de dígitos de n, devuelve el i-ésimo dígito de n.

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = mod (div n (10^(cantDigitos n - i))) 10

cantDigitos :: Integer -> Integer
cantDigitos 0 = 1
cantDigitos n | n < 10 = 1
              | otherwise = 1 + cantDigitos (div n 10)


-- 9 Especificar e implementar una función esCapicua :: Integer -> Bool que dado n ∈ N≥0 determina si n es un número capicúa.

-- ? usar iesimo y cantDigitos

esCapicua :: Integer -> Bool
esCapicua n = posDigito n (cantDigitos n) 1

posDigito :: Integer -> Integer -> Integer -> Bool
posDigito n i c | c > div (cantDigitos n) 2 = True
                | iesimoDigito n i == iesimoDigito n c = posDigito n (i-1) (c+1)
                | otherwise = False


reducirNum :: Integer -> Integer
reducirNum n = div (mod n (10 ^ (cantDigitos n - 1))) 10


-- 10 Especificar, implementar y dar el tipo de las siguientes funciones (símil Ejercicio 4 Práctica 2 de Algebra 1).

f1 :: Int -> Int
f1 0 = 1
f1 n = (2 ^ n) + f1 (n - 1)

f2 :: Int -> Int -> Int
f2 1 _ = 1
f2 n q = (q^n) + f2 (n - 1) q

f3 :: Int -> Int -> Int
f3 n q = f2 (2*n) q


f4 :: Int -> Int -> Int
f4 n q = f3 n q - f2 (n-1) q

-- ¡Atención! A veces ciertas funciones esperan un Float y nosotros tenemos un Int. Para estos casos podemos utilizar la función fromIntegral :: Int ->Float definida en el Preludio de Haskell.
-- 11 a Especificar e implementar una función eAprox :: Integer -> Float que aproxime el valor del número e a partir de la siguiente sumatoria.

eAprox :: Integer -> Float
eAprox 0 = 1
eAprox x = (1 / factorial x) + eAprox (x - 1)

factorial :: Integer -> Float
factorial 0 = 1
factorial n = fromIntegral n * factorial (n - 1)    -- ? fromIntegral


-- 11 b Definir la constante e :: Float como la aproximación de e a partir de los primeros 10 términos de la serie anterior.

e :: Float
e = eAprox 10


-- 12 Especificar e implementar una función raizDe2Aprox :: Integer -> Float que dado n ∈ N devuelva la aproximación de √2 definida por √2 ≈ an−1.

raizDe2Aprox :: Integer -> Float
raizDe2Aprox n = aprox n - 1

aprox :: Integer -> Float
aprox n | n==1 = 2
        | otherwise = 2 + ( 1 / aprox (n-1) )


-- 13 Especificar e implementar la siguiente función

sumatoriaDoble :: Int -> Int -> Int
sumatoriaDoble 0 _ = 0
sumatoriaDoble n m = sumatoriaDoble (n-1) m + sumatoriaInterna n m


sumatoriaInterna :: Int -> Int -> Int
sumatoriaInterna _ 0 = 0
sumatoriaInterna n j = n^j + sumatoriaInterna n (j-1)


-- 14 Especificar e implementar una función sumaPotencias :: Integer ->Integer ->Integer ->Integer que dados tres naturales q, n, m sume todas las potencias de la forma q^a+b con 1 ≤ a ≤ n y 1 ≤ b ≤ m.

sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q 1 b = sumaPotenciasAux q 1 b
sumaPotencias q a b = sumaPotencias q (a-1) b + sumaPotenciasAux q a b


sumaPotenciasAux :: Integer -> Integer -> Integer -> Integer
sumaPotenciasAux q a b  | b == 1 = q^(a+b)
                        | otherwise = sumaPotenciasAux q a (b-1) + q^(a+b)


-- 15 Especificar e implementar una función sumaRacionales :: Integer ->Integer ->Float que dados dos naturales n, m sume todos los número racionales de la forma p/q con 1 ≤ p ≤ n y 1 ≤ q ≤ m

sumaRacionales :: Integer -> Integer -> Float
sumaRacionales 0 _ = 0
sumaRacionales p q = sumaRacionales (p-1) q + sumaRacionalesAux p q

sumaRacionalesAux :: Integer -> Integer -> Float
sumaRacionalesAux p q | q == 1 = fromIntegral p
                      | otherwise = (fromIntegral p/fromIntegral q) + sumaRacionalesAux p (q-1)


-- 16 Recordemos que un entero p > 1 es primo si y sólo si no existe un entero k tal que 1 < k < p y k divida a p.

-- 16a Implementar menorDivisor :: Integer ->Integer que calcule el menor divisor (mayor que 1) de un natural n pasado como parámetro.

menorDivisor :: Integer -> Integer
menorDivisor 1 = 1
menorDivisor n = menorDivisorAux n 2 -- 2 porque pide menor divisor mayor que 1

menorDivisorAux :: Integer -> Integer -> Integer
menorDivisorAux n m | esDivisible n m = m -- m <=> mod n m == 0 => si n primo m <=> m = n
                    | otherwise = menorDivisorAux n (m+1)


-- 16b Implementar la función esPrimo :: Integer ->Bool que indica si un número natural pasado como parámetro es primo.

esPrimo :: Integer -> Bool
esPrimo n = n == menorDivisor n


-- 16c Implementar la función sonCoprimos :: Integer ->Integer ->Bool que dados dos números naturales indica si no tienen algún divisor en común mayor estricto que 1.

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m = sonCoprimosAux n m 2

sonCoprimosAux :: Integer -> Integer -> Integer -> Bool
sonCoprimosAux n m d | d > n || d > m = True
                     | esDivisible n d && esDivisible m d = False
                     | otherwise = sonCoprimosAux n m (d+1)


-- 16d Implementar la función nEsimoPrimo :: Integer -> Integer que devuelve el n-ésimo primo (n ≥ 1). Recordar que el primer primo es el 2, el segundo es el 3, el tercero es el 5, etc.

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo n = nEsimoPrimoAux n 0 2

nEsimoPrimoAux :: Integer -> Integer -> Integer -> Integer
nEsimoPrimoAux n c p | esPrimo p && c == n-1 = p -- CASO BASE
                     | esPrimo p = nEsimoPrimoAux n (c+1) (p+1) -- si p es primo pero el contador no es igual a n pasa
                     | otherwise = nEsimoPrimoAux n c (p+1) -- si p no es primo aumenta p en uno y pasa


-- 17 Implementar la función esFibonacci :: Integer ->Bool

esFibonacci :: Integer -> Bool
esFibonacci n = esFiboAux n 1

esFiboAux :: Integer -> Integer -> Bool
esFiboAux n i | n == fibonacci i = True
               | n < fibonacci i = False
               | otherwise = esFiboAux n (i+1)


-- 18 Implementar una función mayorDigitoPar :: Integer ->Integer

mayorDigitoPar :: Integer -> Integer
mayorDigitoPar n = recorrerDigitos n 1 (-1)

recorrerDigitos :: Integer -> Integer -> Integer -> Integer -- "n"umero, "p"osicion, "m"ayor
recorrerDigitos n p m | p > cantDigitos n = m
                      | mod (iesimoDigito n p) 2 == 0 && iesimoDigito n p > m = recorrerDigitos n (p+1) (iesimoDigito n p)
                      | otherwise = recorrerDigitos n (p+1) m


-- 19 Implementar la función esSumaInicialDePrimos :: Int ->Bool

esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos n = sumaIniPrimos n 0 1

sumaIniPrimos :: Integer -> Integer -> Integer -> Bool
sumaIniPrimos n s c | n == s = True
                    | n < s = False
                    | otherwise = sumaIniPrimos n (sumatoria s c) (c+1)
                        where sumatoria s c = s + nEsimoPrimo c


-- 20 Especificar e implementar la función tomaValorMax :: Int ->Int ->Int que dado un número entero n1 ≥1 y un n2 ≥ n1 devuelve algún m entre n1 y n2 tal que sumaDivisores(m) = max{sumaDivisores(i) |n1 ≤i ≤n2}

-- tomaValorMax :: Int -> Int -> Int
-- tomaValorMax n1 n2 

