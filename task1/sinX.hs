sinX :: Double -> Double
sinXHelper :: Double -> Double -> Double -> Double -> Double

sinXHelper curr n x sum 
    | n > 20 = sum
    | otherwise = sinXHelper (-curr*x*x/((n + 1)*(n + 2))) (n + 2) x (sum + curr)
sinX x = sinXHelper x 1 x 0