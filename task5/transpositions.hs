transpositions:: Eq a => [a] -> [[a]]
transpositions [] = [[]]
transpositions xs = concatMap (\x -> map (x:) $ transpositions $ filter (\y -> y /= x) xs) xs