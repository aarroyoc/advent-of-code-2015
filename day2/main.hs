main :: IO ()
main = do
  file <- readFile "input"
  let sol1 = partOne $ lines file
  putStr "Solution 1: "
  print sol1
  let sol2 = partTwo $ lines file
  putStr "Solution 2: "
  print sol2


partOne :: [String] -> Int
partOne fileLines = sum $ map squareFeet fileLines

partTwo :: [String] -> Int
partTwo fileLines = sum $ map ribbon fileLines

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

squareFeet :: String -> Int
squareFeet str =
  let
    dimensionStr = wordsWhen (=='x') str
    dimensionLength = read (dimensionStr !! 0) :: Int
    dimensionWidth = read (dimensionStr !! 1) :: Int
    dimensionHeight = read (dimensionStr !! 2) :: Int
  in
    2*dimensionLength*dimensionWidth + 2*dimensionWidth*dimensionHeight + 2*dimensionHeight*dimensionLength + smallestSide dimensionLength dimensionWidth dimensionHeight

smallestSide :: Int -> Int -> Int -> Int
smallestSide l w h = min side1 (min side2 side3)
  where
    side1 = l*w
    side2 = l*h
    side3 = w*h

volume :: Int -> Int -> Int -> Int
volume l w h = l*w*h

ribbon :: String -> Int
ribbon str =
  let
    dimensionStr = wordsWhen (=='x') str
    dimensionLength = read (dimensionStr !! 0) :: Int
    dimensionWidth = read (dimensionStr !! 1) :: Int
    dimensionHeight = read (dimensionStr !! 2) :: Int
  in
    smallerRibbon dimensionLength dimensionWidth dimensionHeight + volume dimensionLength dimensionWidth dimensionHeight

smallerRibbon :: Int -> Int -> Int -> Int
smallerRibbon l w h = min path1 (min path2 path3)
  where
    path1 = 2*l + 2*w
    path2 = 2*l + 2*h
    path3 = 2*w + 2*h