mapX :: (a -> b) -> [a] -> [b]
mapX _[] = []
mapX f (x:xs) = f x : mapX f xs

-----------------------------------------
--tail recursion
mapxX :: (a -> b) -> [a] -> [b]
mapHelper :: (a -> b) -> [a] -> [b] -> [b]
mapHelper _ [] acc   = acc
mapHelper f (x:xs) acc = mapHelper f xs (acc ++ [f x])
mapxX f (x:xs) = mapHelper f xs [f x]