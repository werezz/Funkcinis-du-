module TicTacToeStructures where 
data Position = Position {
  coord :: Coord
  , v :: Char
} deriving Show

data Coord = Coord {
  x :: Int
  , y :: Int
} deriving Show

instance Eq Coord where -- kaip lyginti koordinates
  (Coord a1 b1) == (Coord a2 b2) =
    a1 == a2 && b1 == b2

createPositionFromCoord :: Coord -> Char -> Position
createPositionFromCoord coord player = (Position coord player)

getCoordFromPosition :: Position -> Coord
getCoordFromPosition (Position coord _) = coord

getXFromCoord :: Coord -> Int
getXFromCoord (Coord x _) = x

getYFromCoord :: Coord -> Int
getYFromCoord (Coord _ y) = y

getPlayerFromPosition :: Position -> Char
getPlayerFromPosition (Position _ p) = p

--getIntFromIntPosition :: (Position, Int) -> Int
--getIntFromIntPosition (position,number) = number

getIntFromIntPosition :: (Position, Int, Int) -> Int
getIntFromIntPosition (position,number,level) = number

getLevelFromIntPosition :: (Position, Int, Int) -> Int
getLevelFromIntPosition (position,number,level) = level

getPlayerFromIntPosition :: (Position, Int,Int) -> Position
getPlayerFromIntPosition (position,number,level) = position

--getPlayerFromIntPosition :: (Position, Int) -> Position
--getPlayerFromIntPosition (position,number) = position

getInitialPosition :: Position
getInitialPosition = (Position (Coord 1 1 ) 'x')

getAllCoords :: [Coord]
getAllCoords = [(Coord 0 0), (Coord 1 0), (Coord 2 0), (Coord 0 1), (Coord 1 1), (Coord 2 1), (Coord 0 2), (Coord 1 2), (Coord 2 2)]

getFirstRow :: [Coord]
getFirstRow = [(Coord 0 0), (Coord 1 0), (Coord 2 0)]

getSecondRow :: [Coord]
getSecondRow = [(Coord 0 1), (Coord 1 1), (Coord 2 1)]

getThirdRow :: [Coord]
getThirdRow = [(Coord 0 2), (Coord 1 2), (Coord 2 2)]

getFirstCol :: [Coord]
getFirstCol = [(Coord 0 0), (Coord 0 1), (Coord 0 2)]

getSecondCol :: [Coord]
getSecondCol = [(Coord 1 0), (Coord 1 1), (Coord 1 2)]

getThirdCol :: [Coord]
getThirdCol = [(Coord 2 0), (Coord 2 1), (Coord 2 2)]

getFirstDiag :: [Coord]
getFirstDiag = [(Coord 0 0), (Coord 1 1), (Coord 2 2)]

getSecondDiag :: [Coord]
getSecondDiag = [(Coord 0 2), (Coord 1 1), (Coord 2 0)]

getAllCoordCombinations :: [[Coord]]
getAllCoordCombinations = [getFirstRow, getSecondRow, getThirdRow, getFirstCol, getSecondCol, getThirdCol, getFirstDiag, getSecondDiag]

