import Data.Char (digitToInt)
import GHC.Num (integerFromInt)


repeatTimes :: a -> Int -> [a]
repeatTimes _ 0 = []
repeatTimes elem times = elem : repeatTimes elem (times - 1)

getFree :: String -> Int -> [Int]
getFree "" _ = []
getFree (size:rest) index = repeatTimes (-1) (read [size]) ++ getFile rest index

getFile :: String -> Int -> [Int]
getFile "" _ = []
getFile (size:rest) index = repeatTimes index (read [size]) ++ getFree rest (index + 1)

formatInput :: String -> [Int]
formatInput input = getFile input 0

insert :: [Int] -> [Int] -> [Int]
insert [] _ = []
insert first [] = first
insert (x:xs) (y:ys) = if x == -1
                       then y : insert xs ys
                       else x : insert xs (y:ys)

solve :: String -> Int
solve input = sum $ zipWith (*) (take len inserted) [0 .. ]
    where
        inputFormated = formatInput input
        len = length $ filter (/= -1) inputFormated
        inserted = insert inputFormated $ reverse $ filter (/= -1) inputFormated

getFree2 :: String -> Int -> [(Int, Int)]
getFree2 "" _ = []
getFree2 (x:xs) index = (-1, digitToInt x) : getFile2 xs index

getFile2 :: String -> Int -> [(Int, Int)]
getFile2 "" _ = []
getFile2 (x:xs) index = (index, digitToInt x) : getFree2 xs (index + 1)

formatInput2 :: String -> [(Int, Int)]
formatInput2 input = getFile2 input 0

isFree :: (Int, Int) -> Bool
isFree = (==) (-1) . fst

isSmallerEq :: (Int, Int) -> (Int, Int) -> Bool
isSmallerEq a b = snd a <= snd b

sub :: (Int, Int) -> (Int, Int) -> (Int, Int)
sub (t, size1) (_, size2) = (t, size1 - size2)

tryInsert :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
tryInsert [] _ = []
tryInsert (x:xs) elem
    | isFree elem = x : xs
    | isFree x && isSmallerEq elem x = elem : sub x elem : xs
    | otherwise = x : tryInsert xs elem

insert2 :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
insert2 [] _ = []
insert2 lst [] = lst
insert2 lst (x:xs) = insert2 (tryInsert lst x) xs

solve2 :: String -> [(Int, Int)]
solve2 input = insert2 inputFormated $ reverse inputFormated
    where
        inputFormated = formatInput2 input

main :: IO ()
main = do
    input <- readFile "day9.txt"
    let result = solve input
    print result
    let result2 = solve2 input
    print result2