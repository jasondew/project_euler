import Data.List
import System.Environment

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

primesTo :: Int -> [Int]
primesTo m = 2 : sieve [3,5..m]
  where sieve []     = []
        sieve (p:xs) = p : sieve (xs \\ [p*p, p*p+2*p..m])

factor :: Int -> [Int]
factor n = factor' n (primesTo n)
  where factor' 1 _          = []
        factor' n all@(p:ps) = let (d, r) = n `divMod` p
                               in if r == 0
                                  then p : factor' d all
                                  else factor' n ps

divisors :: Int -> [Int]
divisors = map product . init . tail . nub . subsequences . factor

divisorSum :: Int -> Int
divisorSum = sum . divisors

perfect :: Int -> Bool
perfect n = divisorSum n == n

deficient :: Int -> Bool
deficient n = divisorSum n < n

abundant :: Int -> Bool
abundant n = divisorSum n > n

description :: Int -> String
description n = case divisorSum n `compare` n of
                  LT -> "deficient"
                  EQ -> "perfect"
                  GT -> "abundant"

abundants :: [Int]
abundants = [x | x <- [12..], abundant x]

abundantPairsLessThan :: Int -> [(Int, Int)]
abundantPairsLessThan n = [(x, y) | x <- abundantsLessThanN, y <- abundantsLessThanN]
                          where abundantsLessThanN = takeWhile (< n) abundants

nonAbundantPairSummableLessEqual :: Int -> [Int]
nonAbundantPairSummableLessEqual n = [1..n] \\ abundantPairSummableLessEqual n

abundantPairSummableLessEqual :: Int -> [Int]
abundantPairSummableLessEqual n = map (\(x, y) -> x + y) $ abundantPairsLessThan ((n `div` 2) + 1)

main = getArgs >>= print . sum . nonAbundantPairSummableLessEqual . read . head
