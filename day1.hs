import System.IO
import Data.List


getFirstList :: [[Int]] -> [Int]
getFirstList = map head

getSecondList :: [[Int]] -> [Int]
getSecondList = map (!! 1)

getNums :: String -> [Int]
getNums = map read . words

absDiff :: Int -> Int -> Int
absDiff a b = abs $ a - b

solve :: String -> Int
solve inp = sum $ zipWith absDiff lst1 lst2
    where
        lst1 = sort $ getFirstList $ map getNums $ lines inp
        lst2 = sort $ getSecondList $ map getNums $ lines inp

appears :: [Int] -> Int -> Int
appears lst n = length $ filter (== n) lst

solve' :: String -> Int
solve' inp = foldr (f lst2) 0 lst1
    where
        lst1 = sort $ getFirstList $ map getNums $ lines inp
        lst2 = sort $ getSecondList $ map getNums $ lines inp
        f :: [Int] -> Int -> Int -> Int
        f lst new buildUp = buildUp + new * appears lst new

main :: IO ()
main = do
    input <- readFile "day1.txt"

    let res = solve' input

    print res