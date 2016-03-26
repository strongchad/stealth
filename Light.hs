import Data.Foldable (toList)
import Data.Sequence as Sequence

type Coordinate = (Int,Int)

data LightLevel = Drk | Dim | Brt deriving (Eq, Ord)

data Tile = Tile LightLevel -- this datatype stores information about a tile; not about its contents

type Grid = Seq (Seq Tile)

data Direction = North | East | South | West deriving Eq

data LightSource = SquareLight Int Int | Sun Direction



initialGrid :: Grid
initialGrid = Sequence.replicate 20 $ Sequence.replicate 20 $ Tile Drk

test cs = putStrLn $ displayGrid $ foldl (.) id [(addLight (SquareLight 1 3) c)|c <- cs] initialGrid

distance :: Coordinate -> Coordinate -> Int
distance (x1,y1) (x2,y2) = max (abs $ x1-x2) (abs $ y1-y2) --'Moore distance'
--distance (x1,y1) (x2,y2) = sum [(abs $ x1-x2),(abs $ y1-y2)] --Manhattan distance
--distance (x1,y1) (x2,y2) = floor $ sqrt $ fromIntegral $ sum [(x1-x2)*(x1-x2),(y1-y2)*(y1-y2)] --Euclidean distance

brighten :: Tile -> LightLevel -> Tile
brighten (Tile tl) ll = if ll > tl
                        then Tile ll
                        else Tile tl

addLight :: LightSource -> Coordinate -> Grid -> Grid
addLight l@(SquareLight r1 r2) (x,y) tss = ((updateTiles (\c -> if r1 > distance (x,y) c then (\t -> brighten t Brt) else id)) . (updateTiles (\c -> if r2 > distance (x,y) c then (\t -> brighten t Dim) else id))) tss
--addLight l@(Sun d) _ tss = 

--almost
{--
conditionallyBrighten :: Tile -> LightLevel -> (Coordinate -> Bool) -> Tile
conditionallyBrighten t l f = if f
                              then brighten t l
                              else t
--}

updateTile :: Coordinate -> (Tile -> Tile) -> Seq(Seq Tile) -> Grid
updateTile (x,y) f tss = update y (adjust f x (index tss y)) tss

updateTiles :: (Coordinate -> Tile -> Tile) -> Seq(Seq Tile) -> Grid
updateTiles f tss = foldl (.) id [(\tss' -> update y (adjust (f (x,y)) x (index tss' y)) tss') | y <- [0..(Sequence.length tss)-1], x <- [0..Sequence.length (index tss y)-1]] $ tss


-- displaying the grid as a string for testing purposes
displayTile :: Tile -> String
displayTile (Tile l) = displayLightLevel l

displayLightLevel :: LightLevel -> String
displayLightLevel Brt = "██"
displayLightLevel Dim = "▒▒"
displayLightLevel Drk = "░░"

displayGrid :: Grid -> String
displayGrid grid = foldl (++) "" $ map displayRow $ toList grid
  where displayRow xs = '\n':(foldl (++) "" $ map displayTile $ toList xs)
