
incFunc :: Int -> Int -> Bool
incFunc a b = 1 <= (b - a) && (b - a) <= 3

decFunc :: Int -> Int -> Bool
decFunc a b = 1 <= (a - b) && (a - b) <= 3

solveRow :: [Int] -> (Int -> Int -> Bool) -> Bool
solveRow nums f = and $ zipWith f nums (tail nums)

solveRowComplete :: [Int] -> Bool
solveRowComplete nums = solveRow nums incFunc || solveRow nums decFunc

removeAtIndex :: [a] -> Int -> [a]
removeAtIndex lst n = take n lst ++ drop (n+1) lst

solve :: String -> Int
solve inp = length $ filter f numss
    where
        numss = map (map read . words) $ lines inp :: [[Int]]
        f :: [Int] -> Bool
        f nums = solveRowComplete nums || any (solveRowComplete . removeAtIndex nums) [0..length nums]

main :: IO ()
main = do
    input <- readFile "day2.txt"

    let res = solve input

    print res