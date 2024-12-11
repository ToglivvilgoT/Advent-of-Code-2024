import qualified Data.Map as Map
import qualified Data.Set as Set


type AntiNode = (Int, Int)
type Antenna = (Int, Int)
type AntennaGroup = [Antenna]
type AntennaGroups = Map.Map Char AntennaGroup

getAntenna :: Int -> Char -> Int -> AntennaGroups
getAntenna _ '.' _ = Map.empty
getAntenna y typ x = Map.singleton typ [(x, y)]

antennasInRow :: String -> Int -> AntennaGroups
antennasInRow row rowNum = Map.unionsWith (++) $ zipWith (getAntenna rowNum) row [0..]

cartesUnique :: Eq a => [a] -> [a] -> [(a, a)]
cartesUnique [] _ = []
cartesUnique (x : xs) y = filter (uncurry (/=)) $ map (x,) y ++ cartesUnique xs y

getAntiNodePair :: Antenna -> Antenna -> Set.Set AntiNode
getAntiNodePair a b = Set.fromList [node1, node2]
    where
        x1 = fst a
        x2 = fst b
        y1 = snd a
        y2 = snd b
        dx = x1 - x2
        dy = y1 - y2
        node1 = (x1 + dx, y1 + dy)
        node2 = (x2 - dx, y2 - dy)

inBounds :: Int -> Int -> AntiNode -> Bool
inBounds max_x max_y (x, y) = 0 <= x && x < max_x && 0 <= y && y < max_y

getAntiNodes :: Int -> Int -> AntennaGroup -> Set.Set AntiNode
getAntiNodes width height group = Set.filter (inBounds width height) $ Set.unions $ map (uncurry getAntiNodePair) $ cartesUnique group group

solve :: String -> Int
solve input = Set.size $ Set.unions $ Map.elems $ Map.map (getAntiNodes width height) antennaGroups
    where
        width = length $ head $ lines input
        height = length $ lines input
        antennaGroups = Map.unionsWith (++) $ zipWith antennasInRow (lines input) [0..]

getAntiNodeLine :: Int -> Int -> Antenna -> Antenna -> Set.Set AntiNode
getAntiNodeLine max_x max_y (x1, y1) (x2, y2) = Set.fromList $ takeWhile (inBounds max_x max_y) line1 ++ takeWhile (inBounds max_x max_y) line2
    where
        dx = x1 - x2
        dy = y1 - y2
        line1 = map (\n -> (x1 + dx * n, y1 + dy * n)) [0..]
        line2 = map (\n -> (x2 - dx * n, y2 - dy * n)) [0..]

getAntiNodes2 :: Int -> Int -> AntennaGroup -> Set.Set AntiNode
getAntiNodes2 width height group = Set.filter (inBounds width height) $ Set.unions $ map (uncurry (getAntiNodeLine width height)) $ cartesUnique group group

solve2 :: String -> Int
solve2 input = Set.size $ Set.unions $ Map.elems $ Map.map (getAntiNodes2 width height) antennaGroups
    where
        width = length $ head $ lines input
        height = length $ lines input
        antennaGroups = Map.unionsWith (++) $ zipWith antennasInRow (lines input) [0..]

main :: IO ()
main = do
    input <- readFile "day8.txt"
    let result = solve input
    print result
    let result2 = solve2 input
    print result2