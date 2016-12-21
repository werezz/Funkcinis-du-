module MExprParser
where

import TicTacToeStructures

-- Main function

getParsedPositions :: String -> [Position]
getParsedPositions message = getPositions [] message

getParsedPosition :: String -> Position
getParsedPosition message = getFirstPosition(getParsedPositions message)

getFirstPosition :: [Position] -> Position
getFirstPosition (t:h) = t

-- Parsing functions

getPositions :: [Position] -> String -> [Position]
getPositions _ "l[]" = []
getPositions positions message =
    let 
      (position, restFromPosition) = getPosition message
    in
      if restFromPosition == "\"]]"
      then (positions ++ [position])
      else getPositions (positions ++ [position]) restFromPosition


getPosition :: String -> (Position, String) 
getPosition message =
    let
      (coord, restFromCoord) = getCoord message
      (player, restFromPlayer) = getPlayer restFromCoord
    in ((Position coord player), restFromPlayer)

convertToMExpr :: Position -> String
convertToMExpr position =  
  let 
    x = getXFromCoord (getCoordFromPosition position) 
    y = getYFromCoord (getCoordFromPosition position) 
    player = (getPlayerFromPosition position)
  in "l[\"x\"; " ++ (show x) ++ " ; \"y\";" ++ (show y)  ++ "; \"v\"; \"" ++ [player] ++ "\"]"
-- "l[l[\"x\"; 1; \"y\"; 1; \"v\"; \"x\"]]"
-- "l[l[\"x\"; 1 ; \"y\";1; \"v\"; \" 'x'\"]]"

convertToMExprs :: [Position] -> String
convertToMExprs positions =  
  convertToMExprs' ("l[") positions
  where
    convertToMExprs' :: String -> [Position] -> String
    convertToMExprs' convert [] = (convert ++ "]")
    convertToMExprs' convert (h:t) = 
      if (isEmpty t)
       then convertToMExprs' (convert ++ (convertToMExpr h)) t
       else convertToMExprs' (convert ++ (convertToMExpr h) ++ ";") t

getCoord :: String -> (Coord, String) 
getCoord message = 
    let 
      (x, restFromX) = getNumber message
      (y, restFromY) = getNumber restFromX
    in ((Coord x y), restFromY)

getNumber :: String -> (Int, String)
getNumber ('0':rest) = (0, rest)
getNumber ('1':rest) = (1, rest)
getNumber ('2':rest) = (2, rest)
getNumber (c:rest) = getNumber rest

getPlayer :: String -> (Char, String)
getPlayer ('x':rest) = ('x', rest)
getPlayer ('o':rest) = ('o', rest)
getPlayer (c:rest) = getPlayer rest

isEmpty [] = True
isEmpty _ = False

