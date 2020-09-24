primes :: [Int]
sieve :: [Int] -> [Int] 
removeNotPrimes :: [Int] -> [Int] -> [Int]
sieve (x:xs) = x: sieve (removeNotPrimes xs [x*x, x*x + x..])
removeNotPrimes  (x:xs) (y:ys)
    | x < y = x:(removeNotPrimes xs (y:ys))
    | x > y = removeNotPrimes (x:xs) ys
    | otherwise = removeNotPrimes xs ys
primes = 2: sieve [3,5..]