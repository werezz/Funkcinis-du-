{-# LANGUAGE OverloadedStrings #-}
module NetworkManager where

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S
import Network.Http.Client

--buildPostRequest :: S.ByteString -> Request 
--buildPostRequest path = buildRequest1 $ do
--               	http POST (S.concat ["/game/", path])
--                setContentType getContentTypeAccept
--                setHostname getDefaultHostname 80

--buildGetRequest :: S.ByteString -> Request 
--buildGetRequest path = buildRequest1 $ do
--               	http GET (S.concat ["/game/", path])
--                setHostname getDefaultHostname 80
--                setAccept getContentTypeAccept

getContentTypeAccept :: S.ByteString
getContentTypeAccept = "application/m-expr+list"

getDefaultHostname :: String
getDefaultHostname = "http://tictactoe.homedir.eu/game/"

getRequestPath :: String -> String
getRequestPath game = getDefaultHostname ++ game ++ "/player/"

fullRequestPath :: String -> Char -> String
fullRequestPath path player = path ++ [player]

convertToPlayer :: String -> Char 
convertToPlayer (t:h) = t

