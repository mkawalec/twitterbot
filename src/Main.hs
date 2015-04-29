module Main where

import              Network (withSocketsDo)
import              Network.HTTP.Conduit (Request, parseUrl, withManager, httpLbs, responseBody)
import              Data.Conduit
import qualified    Data.ByteString.Lazy as L
import              Data.Maybe
import              Network.HTTP.Types.Header (RequestHeaders)
import qualified    Web.Authenticate.OAuth as OA
import qualified    Data.ByteString.Char8 as BS
import              Data.Default
import              Language.Haskell.TH.Ppr (bytesToString)
import              Control.Monad.IO.Class (liftIO, MonadIO)
import              Control.Monad
import              Data.Aeson
import              Data.Data
import              System.Environment (getEnv)
import qualified    Data.Map as Map
import qualified    Data.Text as T

import              TweetParsers

statusURI = "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="

type Config = Map.Map String String

getConfig :: IO T.Text
getConfig = return . convert =<< L.readFile ".env"
    where convert = T.pack . bytesToString . L.unpack

getConfigMap :: T.Text -> IO Config
getConfigMap config = return $ Map.fromList pairs
    where stringify = T.unpack . T.strip
          pair = \[x, y] -> (stringify x, stringify y)
          processLine = pair . (T.split (=='='))
          correctLines = filter (T.any (=='=')) (T.lines config)
          pairs = map processLine correctLines

parseConfig :: IO Config
parseConfig = getConfigMap =<< getConfig

getMapValue :: Config -> String -> BS.ByteString
getMapValue map key = BS.pack $ map Map.! key 

signRequest :: MonadIO m => Config -> Request -> m Request
signRequest config request = do
    let getConfig = getMapValue config
    let authCred = OA.newCredential (getConfig "ACCESS_TOKEN") (getConfig "ACCESS_TOKEN_SECRET")
    let authConfig = OA.newOAuth { OA.oauthConsumerKey = getConfig "API_KEY"
                                 , OA.oauthConsumerSecret = getConfig "API_SECRET"
                                 }
    OA.signOAuth authConfig authCred request

getTimeline :: Config -> String -> IO (Either String Timeline)
getTimeline config screenName = do
    request <- parseUrl $ statusURI ++ screenName
    res <- withManager $ \m -> do
        signedReq <- signRequest config request
        httpLbs signedReq m
    return $ eitherDecode $ responseBody res

main :: IO ()
main = withSocketsDo $ do
    config <- parseConfig
    res <- getTimeline config "b4zzl3"
    case res of
        Left err -> putStrLn err
        Right ts -> mapM_ print $ take 5 ts
