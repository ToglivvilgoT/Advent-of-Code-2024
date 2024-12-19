import qualified Data.Map as Map


type Stone = Integer
type Amount = Integer
type Stones = Map.Map Stone Amount


format :: String -> Stones
format input = Map.fromList $ map ((,1) . read) (words input)

nextStones :: Stone -> Amount -> Stones
nextStones stone amount
    | stone == 0 = Map.singleton 1 amount
    | even len   = Map.unionWith (+) (Map.singleton (div stone (10 ^ div len 2)) amount) (Map.singleton (mod stone (10 ^ div len 2)) amount)
    | otherwise  = Map.singleton (stone * 2024) amount
    where
        len = length $ show stone

stonesAfter :: Integer -> Stones -> Stones
stonesAfter blinks stones
    | blinks == 0 = stones
    | otherwise   = stonesAfter (blinks - 1) $ Map.unionsWith (+) $ Map.elems $ Map.mapWithKey nextStones stones

solve :: Stones -> Integer -> Integer
solve stones blinks = sum $ Map.elems $ stonesAfter blinks stones

main :: IO ()
main = do
    input <- readFile "day11.txt"
    let result = solve (format input) 25
    print result
    let result2 = solve (format input) 75
    print result2