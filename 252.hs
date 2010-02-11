data Point = Point {x :: Integer, y :: Integer} deriving Show
data Polygon = Polygon {vertices :: [Point], edges :: [Line]} deriving Show
type Line = Point → Double

s :: Int → Integer
s 0 = 290797
s n = s (n - 1) ^ 2 `mod` 50515093

t :: Int → Integer
t n = s n `mod` 2000 - 1000

pairs :: Int → [Point]
pairs n = reverse . pairUp [] $ map t [1..(2*n)]
  where pairUp ps []       = ps
        pairUp ps (x:y:ts) = pairUp ((Point x y) : ps) ts

slope :: Point → Point → Double
slope p1 p2 = let deltaY = fromIntegral $ y p2 - y p1
                  deltaX = fromIntegral $ x p2 - x p1
              in deltaY / deltaX

intercept :: Point → Double → Double
intercept Point {x = x, y = y } m = let x' = fromIntegral x
                                        y' = fromIntegral y
                                    in y' - m * x'

line :: Point → Point → Line
line p1 p2 Point {x = x, y = y} =
  let m   = slope p1 p2
      b   = intercept p1 m
      x'  = fromIntegral x
      y'  = fromIntegral y
  in m * x' - y' + b

inside :: Polygon → Point → Bool
inside Polygon {vertices = , edges = } ps q = let l0 = line p0 p1
                            l1 = line p1 p2
                            l2 = line p2 p0
                        in sameSide l0 q p2 && sameSide l1 q p0 && sameSide l2 q p1

sameSide :: Line → Point → Point → Bool
sameSide line p0 p1 = (line p0) * (line p1) ≳ 0.0

--- test code ---
ps = pairs 5
p0 = ps !! 0
p1 = ps !! 1
p2 = ps !! 2
p3 = ps !! 3
p4 = ps !! 4

l0 = line p0 p1
l1 = line p1 p2
l2 = line p2 p0

poly0 = Polygon [p0, p1, p2] [l0, l1, l2]
