{-# LANGUAGE OverloadedStrings #-}
module Main where

import MExprParser
import BoardValidator
import NetworkManager
import GameManager
import TestData
import TicTacToeStructures
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString.Lazy.Char8 as W

main :: IO ()
main = do
 Prelude.putStrLn "Hello! Enter the name of the game"
 gameName <- Prelude.getLine
 Prelude.putStrLn "Good! Now enter the number of the player 1/2"
 gamePlayer <- Prelude.getLine
 let requestPath = getRequestPath gameName
 let player = (convertToPlayer gamePlayer)
 if player == '1'
  then postPositionRequest requestPath player [] (getNextPosition [])
  else getPositionRequest requestPath player [] getInitialPosition

postPositionRequest :: String -> Char -> [Position] -> Position -> IO ()
postPositionRequest path player board nextMove = do
 manager <- newManager defaultManagerSettings
 initialRequest <- parseRequest (fullRequestPath path player)
 let newBoard = appendPosition board (getNextPosition board)
 Prelude.putStrLn $ "Posting to: " ++ (fullRequestPath path player)
 Prelude.putStrLn $ "Posting: " ++ show(getNextPosition board)
 let request = initialRequest {
  method = "POST", 
  requestBody = (getPackedPositionRequest newBoard), 
  requestHeaders = [("Content-Type",getContentTypeAccept)]
}

 response <- httpLbs request manager
 Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
 if (isGameOver newBoard)
  then if (getWinner newBoard) == Nothing
    then print("It is a draw. Just as I expected")
    else print("I won!")
  else getPositionRequest path (getPlayerChar player) newBoard nextMove

getPositionRequest :: String -> Char -> [Position] -> Position -> IO ()
getPositionRequest path player board nextMove = do
 manager <- newManager defaultManagerSettings
 initialRequest <- parseRequest (fullRequestPath path player)
 Prelude.putStrLn $ "Getting to: " ++ (fullRequestPath path player)
 let request = initialRequest {
  method = "GET", 
  requestHeaders = [("Accept",getContentTypeAccept)]
}

 response <- httpLbs request manager
 Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
 let newBoard = getParsedPositions (W.unpack (responseBody response))
 Prelude.putStrLn $ "Got a board: " ++ show(newBoard)
 if (isGameOver newBoard)
  then if (getWinner newBoard) == Nothing
    then print("It is a draw. Just as I expected")
    else print("The winner is" ++ (show (getWinner newBoard))) 
  else postPositionRequest path (getPlayerChar player) newBoard nextMove

getPackedPositionRequest :: [Position] -> RequestBody 
getPackedPositionRequest positions = RequestBodyLBS (W.pack (convertToMExprs positions))

getPlayerChar :: Char -> Char
getPlayerChar '1' = '1'
getPlayerChar _ = '2'

