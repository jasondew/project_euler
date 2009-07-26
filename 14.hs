import qualified Data.Map as Map

f :: Integer -> Integer
f x | x `mod` 2 == 0 = x `div` 2
f x | otherwise     = 3*x + 1

fr = g 1
  where g i 1 = i
        g i x = g (i+1) (f x)

frIO = g 1
  where g i 1 = return i
        g i x = do
                  print x
                  g (i+1) (f x)

fr' map start = g 1 map start
  where g i map 1 = (Map.insert start i map, i)
        g i map x = case x < 100000 of
                      True  -> g (i+1) map (f x)
                      False -> case Map.lookup x map of
                                Just v  -> (Map.insert start i' map, i') where i' = i + v
                                Nothing -> g (i+1) map (f x)

tfr = g 0 Map.empty
  where g best map min max | min == max = best
        g best map min max | otherwise = case fr' map max of
                                           (map', try) -> case try > best of
                                                           True  -> g try  map' min (max-1)
                                                           False -> g best map' min (max-1)

main = print $ foldr (\n max -> let try = fr n in case try > max of True -> try; False -> max) 0 [1..1000000]
