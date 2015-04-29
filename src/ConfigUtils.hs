module ConfigUtils (
      Config
    , parseConfig
    , getConfigValue
) where

import qualified    Data.Map as Map
import qualified    Data.Text as T
import qualified    Data.ByteString.Char8 as BS
import              Language.Haskell.TH.Ppr (bytesToString)
import qualified    Data.ByteString.Lazy as L

import              Errors
import              Control.Monad.Error
    
type Config = Map.Map String String

getConfig :: String -> IO T.Text
getConfig filename = return . convert =<< L.readFile filename
    where convert = T.pack . bytesToString . L.unpack

getConfigMap :: T.Text -> IO Config
getConfigMap config = return $ Map.fromList pairs
    where stringify = T.unpack . T.strip
          pair = \[x, y] -> (stringify x, stringify y)
          processLine = pair . (T.split (=='='))
          correctLines = filter (T.any (=='=')) (T.lines config)
          pairs = map processLine correctLines

parseConfig :: String -> IO Config
parseConfig file = getConfigMap =<< getConfig file

getConfigValue :: Config -> String -> ThrowsError BS.ByteString
getConfigValue map key = case Map.member key map of
    True -> return $ BS.pack $ map Map.! key 
    False -> throwError $ ConfigError $ "Key " ++ key ++ " does not exist"

