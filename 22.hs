import Data.Char
import Data.List
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec

type Name = String

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = do char '"'
                content <- many quotedChar
                char '"' <?> "quote at end of cell"
                return content

quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

readNames :: Handle -> IO [Name]
readNames h = do
                contents <- hGetContents h
                case parseCSV contents of
                  Left  msg  -> print msg >> return []
                  Right rows -> return $ head rows

charScore :: Char -> Int
charScore c = ord c - 64

nameScore :: Name -> Int
nameScore = foldl (\a c -> a + (charScore c)) 0

totalScore :: [Name] -> Int
totalScore = snd . foldl (\(i, a) s -> (i+1, s*i + a)) (1, 0) . map nameScore . sort

main = do
  (input_filename:_) <- getArgs
  names               <- withFile input_filename ReadMode readNames

  print $ totalScore names
