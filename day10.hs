import qualified Data.Map as Map
import Data.Char (digitToInt)


type Position = (Int, Int)

formatInputLine :: String -> Int -> Map.Map Position Int
formatInputLine line y = Map.fromList $ zipWith (\c x -> ((x, y), digitToInt c)) line [0..]

formatInput :: String -> Map.Map Position Int
formatInput raw = Map.unions $ zipWith formatInputLine (lines raw) [0..]

solve :: Map.Map Position Int -> Int
solve input = 3

main :: IO ()
main = do
    input <- readFile "day10.txt"
    let result = solve $ formatInput input
    print result