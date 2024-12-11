import qualified Data.Map as Map
import qualified Data.Set as Set

type Position = (Int, Int)
type Direction = (Int, Int)

getMapRow :: Int -> String -> Map.Map Position Char
getMapRow rowNum row = Map.fromList $ zip (map (, rowNum) [0..]) row

getMap :: String -> Map.Map Position Char
getMap input = Map.unions $ zipWith getMapRow [0..] (lines input)

getStartPos :: Map.Map Position Char -> (Position, Map.Map Position Char)
getStartPos map = (startPos, newMap)
    where
        startPos = head $ Map.keys $ Map.filter (== '^') map
        newMap = Map.insert startPos '.' map

turn :: (Int, Int) -> (Int, Int)
turn tup = (-y, x)
    where
        x = fst tup
        y = snd tup

tupleAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupleAdd tup1 tup2 = (x1 + x2, y1 + y2)
    where
        x1 = fst tup1
        x2 = fst tup2
        y1 = snd tup1
        y2 = snd tup2

walk :: Map.Map Position Char -> Position -> Direction -> Set.Set Position
walk map pos dir = case Map.lookup nextPos map of
    Nothing       -> Set.empty
    Just nextTile -> if nextTile == '#'
                     then walk map pos (turn dir)
                     else Set.insert nextPos $ walk map nextPos dir
    where
        nextPos = tupleAdd pos dir

solve :: String -> Int
solve input = Set.size $ walk map startPos (0, -1)
    where
        startPos = fst $ getStartPos $ getMap input
        map = snd $ getStartPos $ getMap input

hasLoop :: Map.Map Position Char -> Position -> Direction -> Set.Set (Position, Direction) -> Bool
hasLoop map pos dir visited = case Map.lookup nextPos map of
    Nothing       -> False
    Just nextTile -> if nextTile == '#'
                     then Set.member (pos, turn dir) visited ||
                          hasLoop map pos (turn dir) (Set.insert (pos, turn dir) visited)
                     else Set.member (nextPos, dir) visited ||
                          hasLoop map nextPos dir (Set.insert (nextPos, dir) visited)
    where
        nextPos = tupleAdd pos dir
    
solveOne :: Map.Map Position Char -> Position -> Direction -> Position -> Bool
solveOne map pos dir newObstruct = hasLoop (Map.insert newObstruct '#' map) pos dir (Set.singleton (pos, dir))

solve2 :: String -> Int
solve2 input = Set.size $ Set.filter (solveOne map startPos (0, -1)) $ walk map startPos (0, -1)
    where
        startPos = fst $ getStartPos $ getMap input
        map = snd $ getStartPos $ getMap input

main :: IO ()
main = do
    input <- readFile "day6.txt"

    let result = solve input
    print result

    let result2 = solve2 input
    print result2