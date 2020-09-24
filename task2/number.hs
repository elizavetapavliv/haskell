import Data.List

toBinary :: Int -> [Int]
toBinary n = reverse $ unfoldr (\n-> if n == 0 then Nothing else Just(rem n 2, div n 2)) n

primeDivisors :: Int -> [Int]
primeDivisors n = unfoldr (findDivisors 2) n
findDivisors :: Int -> Int -> Maybe (Int, Int)
findDivisors i n
    | n <= 1 = Nothing
    | rem n i == 0 = Just(i, (div n i))
    | otherwise = findDivisors (i + 1) n
