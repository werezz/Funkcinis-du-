module GameManager where

--message:  l[l["x"; 1; "y"; 1; "v"; "x"]]

-- stack install --extra-include-dirs=/usr/local/opt/openssl/include

-- winner Maybe Char
-- isGameOver:: [Position] -> Bool
-- getNextPosition :: [Position] -> position
-- getAvailableNextCoords

-- append

import TicTacToeStructures
import BoardValidator

--nereikes
import TestData
import MExprParser


-- Main functions

getNextPosition :: [Position] -> Position
getNextPosition [] = getInitialPosition
getNextPosition positions = getPlayerFromIntPosition (performMinMax positions 0)

isGameOver :: [Position] -> Bool
isGameOver [] = False
isGameOver positions = (getWinner positions) /= Nothing || ((tlen positions) == 9)

getWinner :: [Position] -> Maybe Char
getWinner [] = Nothing
getWinner positions =
  if isPlayerWinner positions 'x'
    then Just 'x'
    else 
      if isPlayerWinner positions 'o'
       then Just 'o'
       else Nothing

test :: [Position] 
test = getParsedPositions message

-- Min Max Algorithm

performMinMax :: [Position] -> Int -> (Position, Int, Int)
performMinMax board level = 
  if (getNextPlayer board) == 'x'
        then
          getMinimumLevel $ getMaximums (minMaxIterate board (getAvailableNextPositions board (getNextPlayer board)) level)
        else 
          getMinimumLevel $ getMinimums (minMaxIterate board (getAvailableNextPositions board (getNextPlayer board)) level)

doMinMax :: [Position] -> Int -> [(Position, Int, Int)]
doMinMax board level =
  doMinMax' (getAvailableNextPositions board (getNextPlayer board)) [] level
  where
    doMinMax' :: [Position] -> [(Position, Int, Int)] -> Int -> [(Position, Int, Int)]
    doMinMax' [] states level = states
    doMinMax' (h:t) states level = doMinMax' t (states ++ [(minMax board h level)]) level


minMax :: [Position] -> Position -> Int -> (Position, Int, Int)
minMax board position level =
  if (isGameOver board) 
    then (position, (getScore board), level)
    else 
      if (getNextPlayer board) == 'x'
        then
          (position, (getIntFromIntPosition (getMaximum (minMaxIterate board (getAvailableNextPositions board (getNextPlayer board)) level))), (getLevelFromIntPosition (getMinimumLevel $ getMaximums (minMaxIterate board (getAvailableNextPositions board (getNextPlayer board)) level))))
        else 
          (position, (getIntFromIntPosition (getMinimum (minMaxIterate board (getAvailableNextPositions board (getNextPlayer board)) level))), (getLevelFromIntPosition (getMinimumLevel $ getMinimums (minMaxIterate board (getAvailableNextPositions board (getNextPlayer board)) level))))

getMaximum :: [(Position, Int, Int)] -> (Position, Int, Int)
getMaximum (h:t) =
  getMaximum' (h:t) h
  where 
    getMaximum' :: [(Position, Int, Int)] -> (Position, Int, Int) -> (Position, Int, Int)
    getMaximum' [] maxPosition = maxPosition
    getMaximum' (h:t) maxPosition =
      if (getIntFromIntPosition h) > (getIntFromIntPosition maxPosition)
        then getMaximum' t h
      else 
        getMaximum' t maxPosition 

getMinimum :: [(Position, Int, Int)] -> (Position, Int, Int)
getMinimum (h:t) =
  getMinimum' (h:t) h
  where 
    getMinimum' :: [(Position, Int, Int)] -> (Position, Int, Int) -> (Position, Int, Int)
    getMinimum' [] minimumPosition = minimumPosition
    getMinimum' (h:t) minimumPosition =
      if (getIntFromIntPosition h) < (getIntFromIntPosition minimumPosition)
        then getMinimum' t h
      else 
        getMinimum' t minimumPosition 


getMinimums :: [(Position, Int, Int)] -> [(Position, Int, Int)]
getMinimums (h:t) =
  getMinimums' (h:t) [h] h
  where 
    getMinimums' :: [(Position, Int, Int)] -> [(Position, Int, Int)] ->  (Position, Int, Int) -> [(Position, Int, Int)]
    getMinimums' [] positions minimumPosition = positions
    getMinimums' (h:t) positions minimumPosition =
      if (getIntFromIntPosition h) == (getIntFromIntPosition minimumPosition)
        then getMinimums' t (positions ++ [h]) h
      else if (getIntFromIntPosition h) < (getIntFromIntPosition minimumPosition)
        then getMinimums' t [h] h
      else 
        getMinimums' t positions minimumPosition 

getMaximums :: [(Position, Int, Int)] -> [(Position, Int, Int)]
getMaximums (h:t) =
  getMaximums' (h:t) [h] h
  where 
    getMaximums' :: [(Position, Int, Int)] -> [(Position, Int, Int)] ->  (Position, Int, Int) -> [(Position, Int, Int)]
    getMaximums' [] positions maxPosition = positions
    getMaximums' (h:t) positions maxPosition =
         if (getIntFromIntPosition h) == (getIntFromIntPosition maxPosition)
        then getMaximums' t (positions ++ [h]) h
      else if (getIntFromIntPosition h) > (getIntFromIntPosition maxPosition)
        then getMaximums' t [h] h
      else 
        getMaximums' t positions maxPosition 

getMinimumLevel :: [(Position, Int, Int)] -> (Position, Int, Int)
getMinimumLevel (h:t) =
  getMinimumLevel' (h:t) h
  where 
    getMinimumLevel' :: [(Position, Int, Int)] -> (Position, Int, Int) -> (Position, Int, Int)
    getMinimumLevel' [] minimumPosition = minimumPosition
    getMinimumLevel' (h:t) minimumPosition =
      if (getLevelFromIntPosition h) < (getLevelFromIntPosition minimumPosition)
        then getMinimumLevel' t h
      else 
        getMinimumLevel' t minimumPosition 

minMaxIterate :: [Position] -> [Position] -> Int -> [(Position, Int, Int)]
minMaxIterate _ [] level = [] 
minMaxIterate board moves level =
  minMaxIterate' board moves [] level
  where
    minMaxIterate' :: [Position] -> [Position] -> [(Position, Int, Int)] -> Int -> [(Position, Int, Int)]
    minMaxIterate' _ [] states level = states
    minMaxIterate' board (h:t) states level = minMaxIterate' board t (states ++ [minMax (appendPosition board h) h  $ level + 1]) level

getAvailableNextBoards :: [Position] -> Char -> [[Position]]
getAvailableNextBoards positions player =
  getAvailableNextBoards' [] (getAvailableNextPositions positions player) positions player
  where
    getAvailableNextBoards' :: [[Position]] -> [Position] -> [Position] -> Char -> [[Position]]     
    getAvailableNextBoards' board [] _ _ = board
    getAvailableNextBoards' board (h:t) positions player = getAvailableNextBoards' (board ++ [positions ++ [h]]) t positions player

getAvailableNextPositions :: [Position] -> Char -> [Position]
getAvailableNextPositions positions player =
  getAvailableNextPositions' [] (getAvailableNextCoords positions) player 
  where
    getAvailableNextPositions' :: [Position] -> [Coord] -> Char -> [Position]
    getAvailableNextPositions' newPositions [] _ = newPositions
    getAvailableNextPositions' newPositions (h:t) player = getAvailableNextPositions' (newPositions ++ [(createPositionFromCoord h player)]) t player 

getAvailableNextCoords :: [Position] -> [Coord]
getAvailableNextCoords positions =
  getAvailableNextCoords' [] getAllCoords positions
  where
    getAvailableNextCoords' :: [Coord] -> [Coord] -> [Position] -> [Coord]
    getAvailableNextCoords' coord [] positions = coord
    getAvailableNextCoords' coord (h:t) positions =
      if (countPlayersAtPosition (Position h 'x') positions) > 0
        then getAvailableNextCoords' coord t positions
        else getAvailableNextCoords' (coord ++ [h]) t positions

getScore :: [Position] -> Int
getScore positions =
    if (getWinner positions) == Just 'x'
    then 10
    else 
      if (getWinner positions) == Just 'o'
       then -10
       else 0

getNextPlayer :: [Position] -> Char
getNextPlayer positions = 
  if (countPlayers 'x' positions) == ((countPlayers 'o' positions) + 1)
  then 'o'
  else 'x'

-- Appending

appendPosition :: [Position] -> Position -> [Position]
appendPosition positions position = positions ++ [position]

-- Determining Winner

tlen :: [a] -> Int
tlen l = 
  tlen' 0 l
  where
    tlen' :: Int -> [a] -> Int
    tlen' acc [] = acc
    tlen' acc (h:t) = tlen' (acc + 1) t 

isPlayerWinner :: [Position] -> Char -> Bool
isPlayerWinner [] player = False
isPlayerWinner positions player = 
  isPlayerWinner' False getAllCoordCombinations positions player 
  where
    isPlayerWinner' :: Bool -> [[Coord]] -> [Position] -> Char -> Bool
    isPlayerWinner' isWinner [] _ _ = isWinner
    isPlayerWinner' isWinner (h:t) positions player = isPlayerWinner' ((playerExistAtCoords positions h player) || isWinner) t positions player


playerExistAtCoords :: [Position] -> [Coord] -> Char -> Bool
playerExistAtCoords positions coords player =
  playerExistAtCoords' True positions coords player 
  where 
    playerExistAtCoords' :: Bool -> [Position] -> [Coord] -> Char -> Bool
    playerExistAtCoords' exists [] _ player = False
    playerExistAtCoords' exists positions [] player = exists
    playerExistAtCoords' exists positions (h:t) player = playerExistAtCoords' ((playerExistAtCoord positions h player) && exists) positions t player


playerExistAtCoord :: [Position] -> Coord -> Char -> Bool
playerExistAtCoord positions coord player =
  playerExistAtCoord' positions coord player
  where
    playerExistAtCoord' :: [Position] -> Coord -> Char -> Bool
    playerExistAtCoord' [] coord player = False
    playerExistAtCoord' (h:t) coord player = 
      if ((getCoordFromPosition h) == coord) && (getPlayerFromPosition h) == player
        then True
        else playerExistAtCoord' t coord player