-- 1 Implementar la función fibonacci:: Integer ->Integer que devuelve el i-ésimo número de Fibonacci.

fibonacci:: Integer ->Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)


-- 2 Implementar una función parteEntera :: Float ->Integer

parteEntera :: Float -> Integer
parteEntera n = floor n


-- 3 Especificar e implementar la función esDivisible :: Integer -> Integer -> Bool que dados dos números naturales determinar si el primero es divisible por el segundo. 

esDivisible :: Integer -> Integer -> Bool
esDivisible 0 _ = True
esDivisible a b | a < b = False
                | otherwise = esDivisible (a - b) b


-- 4 Especificar e implementar la función sumaImpares :: Integer ->Integer que dado n ∈ N sume los primeros n números impares

sumaImpares :: Integer ->Integer
sumaImpares 1 = 1
sumaImpares 0 = 0
sumaImpares n | mod n 2 == 0 = sumaImpares (n - 1)
              | n < 0 = 0
              | otherwise = n + sumaImpares (n - 2)


-- 5 Implementar la función medioFact :: Integer ->Integer que dado n ∈ N calcula n!! = n (n−2)(n−4)· · · .

medioFact :: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact n | n < 0 = n
            | otherwise = n * medioFact (n - 2)
