import Data.Map (Map)
import qualified Data.Map as Map

data Direction = DUp | DDown | DLeft | DRight
data Pos = MkPos Int Int

instance Eq Pos where
  (MkPos x1 y1) == (MkPos x2 y2) = x1 == x2 && y1 == y2

instance Ord Pos where
  compare (MkPos x1 y1) (MkPos x2 y2) = compare (x1, y1) (x2, y2) 

instance Show Pos where
  show (MkPos x y) = "(" ++ (show x) ++ "," ++ (show y) ++ ")"

main :: IO ()
main = do
  file <- readFile "input"
  let sol1 = partOne file
  putStr "Solution 1: "
  print sol1
  let sol2 = partTwo file
  putStr "Solution 2: "
  print sol2

partOne :: String -> Int
partOne str =
  let
    directions = parse str
    houses = visitHouses directions
  in
    Map.size houses

partTwo :: String -> Int
partTwo str =
  let
    allDirections = zip [0..] $ parse str
    roboDirections = map (\(x,y) -> y) $ filter (\(x,_) -> odd x) allDirections
    elfDirections = map (\(x,y) -> y) $ filter (\(x, _) -> even x) allDirections
  in
    Map.size $ visitHouses' (MkPos 0 0) elfDirections (visitHouses roboDirections)

parse :: String -> [Direction]
parse = map signToDirection

signToDirection :: Char -> Direction
signToDirection c =
  case c of
    '^' -> DUp
    '>' -> DRight
    '<' -> DLeft
    'v' -> DDown

visitHouses :: [Direction] -> Map Pos Int
visitHouses direction = visitHouses' (MkPos 0 0) direction Map.empty

visitHouses' :: Pos -> [Direction] -> Map Pos Int -> Map Pos Int
visitHouses' pos [] houses = visitHouse pos houses
visitHouses' (MkPos x y) (d:ds) houses = visitHouses' nextHouse ds (visitHouse (MkPos x y) houses) 
  where
    nextHouse = case d of
                 DUp -> MkPos x (y+1)
		 DDown -> MkPos x (y-1)
		 DLeft -> MkPos (x-1) y
		 DRight -> MkPos (x+1) y


visitHouse :: Pos -> Map Pos Int -> Map Pos Int
visitHouse currentHouse houses =
  case Map.lookup currentHouse houses of
    Just gifts -> Map.insert currentHouse (gifts + 1) houses
    Nothing -> Map.insert currentHouse 1 houses    

    