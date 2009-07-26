primaries = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
irregulars = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
secondaries = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

ones = primaries

hundreds = irregulars ++ (concat $ map f secondaries)
  where f secondary = reverse $ foldl (\all x -> (secondary ++ x) : all) [secondary] primaries

thousands = concat $ map f primaries
  where f primary = [primary ++ "hundred"] ++ (map (\number-> primary ++ "hundredand" ++ number) $ ones ++ hundreds)

numbers = ones ++ hundreds ++ thousands ++ ["onethousand"]

main = print . length . concat $ numbers
