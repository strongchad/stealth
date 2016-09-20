type Coordinate = (Int,Int)

distance :: Coordinate -> Coordinate -> Int
distance (x1,y1) (x2,y2) = max (abs $ x2-x1) (abs $ y2-y1) --Moore distance

data Light = Light Int -- Bright distance
                   Int -- Dim distance

data Board = Board Int Int [(Light,Coordinate)]

data Brightness = Dark
                | Dim
                | Bright
  deriving (Eq,Ord,Show)

tileBrightness :: Board -> Coordinate -> Brightness
tileBrightness (Board _ _ ls) c1 = maximum $ map eachLightBrightness ls
  where eachLightBrightness ((Light brt dim), c2) = case (compare brt $ distance c1 c2, compare dim $ distance c1 c2) of
                                                           (GT,_ ) -> Bright
                                                           (EQ,_ ) -> Bright
                                                           (LT,GT) -> Dim
                                                           (LT,EQ) -> Dim
                                                           (LT,LT) -> Dark
drawTile :: Brightness -> String
drawTile Dark   = " "
drawTile Dim    = "▒"
drawTile Bright = "█"

drawBoard :: Board -> String
drawBoard b@(Board width height _) = unlines $ map (\y -> concatMap (drawTile . tileBrightness b) [(x,y)|x <- [0..width]]) [0..height]

main = putStr $ drawBoard $ Board 15 15 [((Light 2 4),(4,4)), ((Light 2 4),(10,10))]
