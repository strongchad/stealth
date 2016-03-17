import Data.Foldable (toList)
import Data.Sequence as Sequence

type Coordinate = (Int,Int)

data LightLevel = Drk | Dim | Brt deriving (Eq, Ord)

data Tile = Tile LightLevel -- this datatype stores information about a tile; not about its contents

data Grid = Grid (Seq (Seq Tile)) [(LightSource, Coordinate)]

data LightSource = SquareLight Int Int


distance :: Coordinate -> Coordinate -> Int
distance (x1,y1) (x2,y2) = max (abs $ x1-x2) (abs $ y1-y2) --'Moore distance'
--distance (x1,y1) (x2,y2) = sum [(abs $ x1-x2),(abs $ y1-y2)] --Manhattan distance
--distance (x1,y1) (x2,y2) = floor $ sqrt $ fromIntegral $ sum [(x1-x2)*(x1-x2),(y1-y2)*(y1-y2)] --Euclidean distance

initialGrid :: Grid
initialGrid = Grid (Sequence.replicate 20 $ Sequence.replicate 20 $ Tile Drk) []

test c@(x,y) = addLight (SquareLight 2 3) c initialGrid

brighten :: Tile -> LightLevel -> Tile
brighten (Tile tl) ll = if ll > tl
                        then Tile ll
                        else Tile tl

addLight :: LightSource -> Coordinate -> Grid -> Grid
addLight l@(SquareLight r1 r2) (x,y) (Grid tss ls) = Grid (((updateTiles (\c -> if r1 > distance (x,y) c then (\t -> brighten t Brt) else id)) . (updateTiles (\c -> if r2 > distance (x,y) c then (\t -> brighten t Dim) else id))) tss) $ (l,(x,y)):ls

updateTile :: Coordinate -> (Tile -> Tile) -> Seq(Seq Tile) -> Seq(Seq Tile)
updateTile (x,y) f tss = update y (adjust f x (index tss y)) tss

updateTiles :: (Coordinate -> Tile -> Tile) -> Seq(Seq Tile) -> Seq(Seq Tile)
updateTiles f tss = foldl (.) id [(\tss' -> update y (adjust (f (x,y)) x (index tss' y)) tss') | y <- [0..(Sequence.length tss)-1], x <- [0..Sequence.length (index tss y)-1]] $ tss


-- displaying the grid as a string for testing purposes
instance Show Tile where
  show (Tile l) = show l

instance Show LightLevel where
  show Brt = "██"
  show Dim = "▒▒"
  show Drk = "░░"

instance Show Grid where
  show (Grid grid _) = foldl (++) "" $ map showRow $ toList grid
    where showRow xs = '\n':(foldl (++) "" $ map show $ toList xs)
