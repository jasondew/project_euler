import Data.List
import Data.Map
import Data.Maybe
import System.Environment
import System.IO

type Row = [Int] 

readTriangleFormat :: Handle -> IO [Row]
readTriangleFormat = g []
  where g rows h = do
                     eof <- hIsEOF h
                     case eof of
                       True  -> return rows
                       False -> do
                         line <- hGetLine h
                         g (parseRow line : rows) h
  
parseRow :: String -> Row
parseRow = f []
  where f row [] = row
        f row s  = let (a, b) = span (/= ' ') s
                       rest   = dropWhile (== ' ') b
                   in f (read a : row) rest

optimalScore :: [Row] -> Int
optimalScore (row:rows) = f row rows
  where f maximums (last_row:[]) = (head last_row) + (maximum maximums)
        f maximums (row:rows)    = f (newMaximums row maximums) rows

newMaximums []     _          = []
newMaximums (x:xs) (m1:m2:ms) = (x + (maximum [m1, m2])) : (newMaximums xs (m2:ms))

main :: IO ()
main = do
  (input_filename:_) <- getArgs
  rows               <- withFile input_filename ReadMode readTriangleFormat

--  print rows
  print $ optimalScore rows
