module Errors where

import              Control.Monad.Error

data BotError = ParseError String
              | ConfigError String
              | ConnectionError String
              | Default String

showError :: BotError -> String
showError (ParseError msg) = "Error during parsing: " ++ msg
showError (ConfigError msg) = "Error when parsing config: " ++ msg
showError (ConnectionError msg) = "Error when connecting: " ++ msg
showError (Default msg) = "Error: " ++ msg

instance Show BotError where show = showError 
instance Error BotError where
    noMsg = Default "We have an error :("
    strMsg = Default

type ThrowsError = Either BotError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
