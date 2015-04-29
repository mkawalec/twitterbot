module Main where

import              Network (withSocketsDo)
import              Network.HTTP.Conduit (Request, parseUrl, withManager, httpLbs, responseBody)
import              Data.Conduit
import              Data.Maybe
import              Network.HTTP.Types.Header (RequestHeaders)
import qualified    Web.Authenticate.OAuth as OA
import              Data.Default
import              Control.Monad.IO.Class (liftIO, MonadIO)
import              Control.Monad
import              Data.Aeson
import              Data.Data

import              TweetParsers
import              ConfigUtils
import              Errors
import              Control.Monad.Error

statusURI = "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="

signRequest :: MonadIO m => Config -> Request -> ThrowsError (m Request)
signRequest config request = do
    let getConfig = getConfigValue config
    token <- getConfig "ACCESS_TOKEN"
    tokenSecret <- getConfig "ACCESS_TOKEN_SECRET"
    apiKey <- getConfig "API_KEY"
    apiSecret <- getConfig "API_SECRET"

    let authCred = OA.newCredential token tokenSecret
    let authConfig = OA.newOAuth { OA.oauthConsumerKey = apiKey
                                 , OA.oauthConsumerSecret = apiSecret
                                 }
    return $ OA.signOAuth authConfig authCred request

getTimeline :: Config -> String -> IO (ThrowsError Timeline)
getTimeline config screenName = do
    request <- parseUrl $ statusURI ++ screenName
    res <- withManager $ \m -> do
        case signRequest config request of
            Left err -> return $ err
            Right req -> do
                signedReq <- req
                return $ Right $ httpLbs signedReq m

    case res of
        Left err -> return $ throwError err
        Right response -> case eitherDecode $ responseBody response of
            Left err -> return $ throwError $ ParseError err
            Right decoded -> return $ return decoded

main :: IO ()
main = withSocketsDo $ do
    config <- parseConfig ".env"
    res <- getTimeline config "b4zzl3"
    case res of
        Left err -> putStrLn $ show err
        Right ts -> mapM_ print $ take 5 ts
