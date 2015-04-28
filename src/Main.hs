module Main where

import              Network (withSocketsDo)
import              Network.HTTP.Conduit
import qualified    Data.ByteString.Lazy as L
import              Data.Maybe
import              Network.HTTP.Types.Header (RequestHeaders)
import qualified    Web.Authenticate.OAuth as OA
import qualified    Data.ByteString.Char8 as BS
import              Data.Default
import              Language.Haskell.TH.Ppr (bytesToString)
import              Control.Monad.IO.Class (liftIO)
import              Control.Monad
import              Data.Aeson
import              Data.Data
import              System.Environment (getEnv)
import qualified    Data.Map as Map
import qualified    Data.Text as T

import              TweetParsers

statusURI = "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=b4zzl3"

getConfig :: IO T.Text
getConfig = return . convert =<< L.readFile ".env"
    where convert = T.pack . bytesToString . L.unpack

getConfigMap :: T.Text -> IO (Map.Map String String)
getConfigMap config = return $ Map.fromList pairs
    where stringify = T.unpack . T.strip
          pair = \[x, y] -> (stringify x, stringify y)
          processLine = pair . (T.split (=='='))
          correctLines = filter (T.any (=='=')) (T.lines config)
          pairs = map processLine correctLines

parseConfig :: IO (Map.Map String String)
parseConfig = getConfigMap =<< getConfig

parseResponse :: L.ByteString -> IO Timeline
parseResponse body = case decode body of
    Just entries -> return entries
    Nothing -> fail "dupa"

getMapValue :: Map.Map String String -> String -> BS.ByteString
getMapValue map key = BS.pack $ map Map.! key 

main :: IO ()
main = withSocketsDo $ do
    config <- parseConfig
    let getConfig = getMapValue config
    let authCred = OA.newCredential (getConfig "ACCESS_TOKEN") (getConfig "ACCESS_TOKEN_SECRET")
    let authConfig = OA.newOAuth { OA.oauthConsumerKey = getConfig "API_KEY"
                                 , OA.oauthConsumerSecret = getConfig "API_SECRET"
                                 }
    
    request <- parseUrl statusURI
    signedReq <- OA.signOAuth authConfig authCred request
    res <- withManager $ httpLbs signedReq
    body <- parseResponse (responseBody res)
    putStr $ show body
    
