import Data.Function

mapX :: (a -> b) -> [a] -> [b]
mapX = fix $ \mapRec f xs ->
    case xs of
        [] -> []
        (x:xs) -> f x : mapRec f xs