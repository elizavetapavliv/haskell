foldrX :: (a -> b -> b) -> b -> [a] -> b
foldrX _ acc [] = acc
foldrX f acc (x:xs) = f x (foldrX f acc xs)

foldlX :: (b -> a -> b) -> b -> [a] -> b
foldlX _ acc [] = acc
foldlX f acc (x:xs) = foldlX f (f acc x) xs

--foldlX выполняется дольше foldrX, несмотря на хвостовую рекурсию, из-за ленивого вычисления в foldrX