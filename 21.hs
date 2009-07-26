import Data.List hiding (map)

type Pair = (Int, Int)

divides a b = a `mod` b == 0

divisorsSum :: Int -> Int
divisorsSum 0 = 0
divisorsSum 1 = 1
divisorsSum n = foldl f 1 [2..maxDivisor]
  where maxDivisor = n `div` 2
        f s x | n `divides` x = s + x
        f s _ | otherwise     = s

amicable :: Pair -> Pair -> Bool
amicable x y | fst x == fst y = False
amicable x y | otherwise     = fst x == snd y && snd x == fst y

amicablePairsUnder :: Int -> [(Pair, Pair)]
amicablePairsUnder n = f [] m
  where ns = [1..n]
        m = map (\n -> (n, divisorsSum n)) ns
        f ps (_:[]) = ps
        f ps (x:xs) = f (ps ++ g x xs) xs
        g x [] = []
        g x (y:ys) | amicable x y = (x,y) : (g x ys)
        g x (y:ys) | otherwise    = g x ys

main = print . sum . concat . map (\x -> let p = fst x in [fst p, snd p]) $ amicablePairsUnder 10000
