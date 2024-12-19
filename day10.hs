import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (digitToInt)
import Data.Maybe (isNothing)


type Position = (Int, Int)

formatInputLine :: String -> Int -> Map.Map Position Int
formatInputLine line y = Map.fromList $ zipWith (\c x -> ((x, y), digitToInt c)) line [0..]

formatInput :: String -> Map.Map Position Int
formatInput raw = Map.unions $ zipWith formatInputLine (lines raw) [0..]

getNeighbours :: Position -> [Position]
getNeighbours (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

getTrailHeads :: Map.Map Position Int -> Int -> Position -> (Set.Set Position, Int)
getTrailHeads grid prevHeight pos 
    | isNothing maybeHeight || height - prevHeight /= 1 = (Set.empty, 0)
    | height == 9 = (Set.singleton pos, 1)
    | otherwise = (Set.unions trailHeads, sum trails)
    where
        maybeHeight = Map.lookup pos grid
        (Just height) = maybeHeight
        trailHeads = map (fst . getTrailHeads grid (prevHeight + 1)) (getNeighbours pos)
        trails = map (snd . getTrailHeads grid (prevHeight + 1)) (getNeighbours pos)


solve :: Map.Map Position Int -> (Int, Int)
solve grid = (sum trailHeads, sum trails)
    where
        startPoses = Map.keys $ Map.filter (== 0) grid
        trailHeads = map (Set.size . fst . getTrailHeads grid (-1)) startPoses
        trails = map (snd . getTrailHeads grid (-1)) startPoses

main :: IO ()
main = do
    input <- readFile "day10.txt"
    let result = solve $ formatInput input
    print result