
-- | Helper function for cross
crossLine :: [a] -> b -> [(a, b)]
crossLine lst elem = map (,elem) lst

-- | Returns the cross product of two lists
cross :: [a] -> [b] -> [(a, b)]
cross xs = concatMap (crossLine xs)

-- | Safely access list at index, if index is negative or out of bounds, return Nothing
safeIndex :: [a] -> Int -> Maybe a
safeIndex lst i
    | i < 0             = Nothing
    | null (drop i lst) = Nothing
    | otherwise         = Just $ lst !! i

-- | word -> matrix of letters -> x -> y -> dx -> dy -> 1 if word exists, 0 otherwise.
-- x and y is where the word starts and dx dy is the direction the word goes in.
findWord :: String -> [String] -> Int -> Int -> Int -> Int -> Int
findWord "" _ _ _ _ _ = 1
findWord word letters x y dx dy = case safeIndex letters y of
    Nothing  -> 0
    Just row -> case safeIndex row x of
        Nothing     -> 0
        Just letter -> if letter == head word
                       then findWord (tail word) letters (x + dx) (y + dy) dx dy
                       else 0

-- | Given a matrix of letters and the x y start position of the word, return amount of instances
-- of the word "XMAS" in any of the eight orientations.
solveOne :: [String] -> Int -> Int -> Int
solveOne letters y x = sum $ map (uncurry $ findWord "XMAS" letters x y) $ cross [-1, 0, 1] [-1, 0, 1]

-- | Given a matrix of letters, return the amount of occurences of the word "XMAS"
solve :: [String] -> Int
solve inp = sum $ map (uncurry $ solveOne inp) $ cross [0..length inp - 1] [0..length (head inp) - 1]

main :: IO ()
main = do
    input <- readFile "day4.txt"

    let result = solve $ lines input

    print result