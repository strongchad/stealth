type Coordinate = (Int,Int)

data LightLevel = Bright | Mid | Dark deriving Show

data Tile = Tile LightLevel

instance Show Tile where
show (Tile Bright) = "2"
show (Tile Mid)    = "1"
show (Tile Dark)   = "0"

type Grid = [[Tile]]

