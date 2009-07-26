import Data.List

isqrt = floor . sqrt . fromIntegral

divisors 1 = [1]
divisors n = [1] ++ middles ++ [n]
  where middles = filter (divides n) [2..(n `div` 2)]

numDivisors 1 = 1
numDivisors n = case sqrtN * sqrtN == n of
                  True  -> 2 * lowerFactors - 1
                  False -> 2 * lowerFactors
                where lowerFactors = foldl (\s d -> if (n `divides` d) then s + 1 else s) 1 [2..sqrtN]
                      sqrtN        = isqrt n

divides a b = a `mod` b == 0

triangleNumbers = map triangleNumber [1..]

triangleNumber :: Integer -> Integer
triangleNumber n = (n * (n+1)) `div` 2

f n = find (\x -> (numDivisors x) > n) triangleNumbers

main = print $ f 500
