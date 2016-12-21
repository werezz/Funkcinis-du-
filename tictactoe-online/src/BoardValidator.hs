module BoardValidator where
import TicTacToeStructures
	-- Validation

validate :: [Position] -> Bool
validate positions = 
    validateEqualNumberOfMoves positions
 && validatePositions positions

validateEqualNumberOfMoves :: [Position] -> Bool -- jei x vienu daugiau ar lygiai su o - lenta valydi
validateEqualNumberOfMoves positions =
    (countPlayers 'x' positions) == (countPlayers 'o' positions)
    || (countPlayers 'x' positions) == ((countPlayers 'o' positions) + 1) 


validatePositions :: [Position] -> Bool -- ar nera dvieju ar daugiau zaideju toje pacioje pozicijoje
validatePositions positions =
  validatePositions' True positions
  where
    validatePositions' :: Bool -> [Position] -> Bool 
    validatePositions' valid [] = valid
    validatePositions' valid (h:t) =
      if (countPlayersAtPosition h positions) > 1 
        then validatePositions' False t
        else validatePositions' valid t
        
-- Counters

countPlayersAtPosition :: Position -> [Position] -> Int
countPlayersAtPosition position positions =
    countPlayersAtPosition' 0 position positions
    where
      countPlayersAtPosition' :: Int -> Position -> [Position] -> Int
      countPlayersAtPosition' acc position [] = acc
      countPlayersAtPosition' acc position (h:t) =
        if (getCoordFromPosition position) == (getCoordFromPosition h)
          then countPlayersAtPosition' (acc + 1) position t
          else countPlayersAtPosition' acc position t


countPlayers :: Char -> [Position] -> Int
countPlayers player positions = 
    countPlayers' 0 player positions 
    where
      countPlayers' :: Int -> Char -> [Position] -> Int
      countPlayers' acc player [] = acc
      countPlayers' acc player (h:t) =
        if (getPlayerFromPosition h) == player
          then countPlayers' (acc + 1) player t
          else countPlayers' acc player t


