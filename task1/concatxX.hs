concatxX::[[a]]->[a]
concatxX [] = []
concatxX ([]:ys) = concatxX ys
concatxX ((x:xs):ys) = x:concatxX (xs:ys)

--------------------------------
---tail recursion 

concatxX'::[[a]]->[a]
concatxXHelper:: [[a]] -> [a] -> [a]
concatxXHelper [] acc = reverse(acc)
concatxXHelper ([]:ys) acc = concatxXHelper ys acc
concatxXHelper ((x:xs):ys) acc = concatxXHelper (xs:ys) (x:acc)
concatxX' xs = concatxXHelper xs []