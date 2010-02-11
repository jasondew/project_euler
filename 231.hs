cof :: [Double]
cof = [76.18009172947146,-86.50532032941677,24.01409824083091,-1.231739572450155,0.001208650973866179,-0.000005395239384953]
 
ser :: Double
ser = 1.000000000190015
 
gammaln :: Double -> Double
gammaln xx = let tmp' = (xx+5.5) - (xx+0.5)*log(xx+5.5)
                 ser' = foldl (+) ser $ map (\(y,c) -> c/(xx+y)) $ zip [1..] cof
             in -tmp' + log(2.5066282746310005 * ser' / xx)

factorial 0 = 1
factorial n = n * factorial (n - 1)

choose :: Integer -> Integer -> Double
n `choose` k = exp $ gammaln (fromIntegral $ n+1) - gammaln (fromIntegral $ k+1) - gammaln (fromIntegral $ n-k+1)
