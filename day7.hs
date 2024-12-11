


solveEquation :: Int -> [Int] -> Bool
solveEquation expected [result] = expected == result
solveEquation expected (a:b:rest) = solveEquation expected (a + b:rest) ||
                                    solveEquation expected (a * b:rest)

solveOne :: String -> Int
solveOne line = if solveEquation result numbers then result else 0
    where
        result = read $ takeWhile (/= ':') line :: Int
        numbers = map read $ words $ drop 2 $ dropWhile (/= ':') line :: [Int]

solve :: String -> Int
solve = sum . map solveOne . lines

solveEquation2 :: Int -> [Int] -> Bool
solveEquation2 expected [result] = expected == result
solveEquation2 expected (a:b:rest) = solveEquation2 expected (a + b:rest) ||
                                     solveEquation2 expected (a * b:rest) ||
                                     solveEquation2 expected(read (show a ++ show b):rest)

solveOne2 :: String -> Int
solveOne2 line = if solveEquation2 result numbers then result else 0
    where
        result = read $ takeWhile (/= ':') line :: Int
        numbers = map read $ words $ drop 2 $ dropWhile (/= ':') line :: [Int]

solve2 :: String -> Int
solve2 = sum . map solveOne2 . lines

main :: IO ()
main = do
    input <- readFile "day7.txt"
    let result = solve input
    print result
    let result2 = solve2 input
    print result2