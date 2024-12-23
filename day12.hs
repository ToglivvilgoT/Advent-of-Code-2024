import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (genericDrop)
import Distribution.Simple.Program.HcPkg (RegisterOptions(RegisterOptions))


type Position = (Int, Int)
type Grid = Map.Map Position Char
type Region = Set.Set Position


getRegions :: Grid -> [Region]
getRegions grid
    | Map.null grid = []
    | otherwise     = region : getRegions (grid \\ region)
    where 
        region = getRegion $ Map.elemAt 0 grid

        getRegion :: (Position, Char) -> Region
        getRegion ((x, y), c) = 

format :: String -> Grid
format input = getRegions grid
    where
        grid = Map.unions $ zipWith formatLine (lines input) [0..]

        formatLine :: String -> Int -> Grid
        formatLine line y = Map.fromList $ zipWith (formatTile y) line [0..]

        formatTile :: Int -> Char -> Int -> (Position, Char)
        formatTile y t x = ((x, y), t)

solve :: Grid

main :: IO ()
main = do
    input <- readFile "day12.txt"
    let result = solve $ format input
    print result