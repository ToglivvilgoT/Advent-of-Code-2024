import Data.Char (isDigit)


samePrefix :: Eq a => [a] -> [a] -> Bool
samePrefix [] _ = True
samePrefix _ [] = True
samePrefix (x:xs) (y:ys) = x == y && samePrefix xs ys

multiply5 :: String -> Int
multiply5 "" = 1
multiply5 (x:xs) = if x == ')' then 1 else 0

multiply4 :: String -> Int
multiply4 str = read (takeWhile isDigit str) * multiply5 (dropWhile isDigit str)

multiply3 :: String -> Int
multiply3 str = if samePrefix "," str
                then multiply4 $ drop 1 str
                else 0

multiply2 :: String -> Int
multiply2 str = read (takeWhile isDigit str) * multiply3 (dropWhile isDigit str)

multiply :: String -> Int
multiply str = if samePrefix "mul(" str
               then multiply2 $ drop 4 str
               else 0

solveLine :: String -> Int
solveLine "" = 0
solveLine line = multiply line + solveLine (tail line)

solve :: String -> Int
solve = sum . map solveLine . lines 

dropUntilDo :: String -> String
dropUntilDo ('d':'o':'(':')':rest) = rest
dropUntilDo "" = ""
dropUntilDo (x:xs) = dropUntilDo xs

solve2 :: String -> Int
solve2 "" = 0
solve2 line = if samePrefix "don't()" line
                  then solve2 $ dropUntilDo line
                  else multiply line + solve2 (tail line)

main :: IO ()
main = do
    input <- readFile "day3.txt"
    let result = solve input
    print result
    let result2 = solve2 input
    print result2