type Rules = [[String]]
type Update = [String]

-- | Returns the tail of list, if list is empty return empty list
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

-- | splits a string on every instance of char. Char is removed from resulting list
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c str = takeWhile (/= c) str : splitOn c (safeTail $ dropWhile (/= c) str)

-- | Given rules and an update returns True if update follows rules, False otherwise
updateInOrder :: Rules -> Update -> Bool
updateInOrder _ [] = True
updateInOrder rules (page:pages) = not (any (\p -> elem p $ map head $ filter (\r -> page == r !! 1) rules) pages) && rest
    where
        rest = updateInOrder rules pages

-- | Returns the middle element of a list
middleElem :: [a] -> a
middleElem lst = lst !! div (length lst) 2

-- | Given an imput, solves part 1 and returns the answer
solve :: String -> Int
solve input = sum $ map (read . middleElem) $ filter (updateInOrder rules) updates
    where
        rules = map (splitOn '|') $ takeWhile (/= "") $ lines input
        updates = map (splitOn ',') $ safeTail $ dropWhile (/= "") $ lines input

-- | Given a set of rules and 2 sorted updates, return a new sorted update
-- | composed of the 2 input updates
merge :: Rules -> Update -> Update -> Update
merge _ [] lst = lst
merge _ lst [] = lst
merge rules (x:xs) (y:ys) = if [x, y] `elem` rules
                            then x : merge rules xs (y:ys)
                            else y : merge rules (x:xs) ys

-- | Given a set of rules and an update, return the update sorted
sortUpdate :: Rules -> Update -> Update
sortUpdate _ [] = []
sortUpdate _ [e] = [e]
sortUpdate rules lst = merge rules left right
    where
        left = sortUpdate rules $ take (div (length lst) 2) lst
        right = sortUpdate rules $ drop (div (length lst) 2) lst

-- | Given an input, solve part 2 and return the answer
solve2 :: String -> Int
solve2 input = sum $ map (read . middleElem . sortUpdate rules) $ filter (not . updateInOrder rules) updates
    where
        rules = map (splitOn '|') $ takeWhile (/= "") $ lines input
        updates = map (splitOn ',') $ safeTail $ dropWhile (/= "") $ lines input

main :: IO ()
main = do
    input <- readFile "day5.txt"

    let result = solve input
    print result

    let result2 = solve2 input
    print result2