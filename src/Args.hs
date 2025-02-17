module Args (parseArgs, assignValue, validateRule, Token(..), safeTail) where

import Control.Exception (throwIO)
import System.Environment
import Text.Read (readMaybe)
import ErrorHandling (invalidOptionError, handleHelpException, exitError)
import Display

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

data Token = RULE Int | START Int | WINDOW Int | LINES Int | MOVE Int deriving (Eq, Show)

parseArgs :: [String] -> Either String [Token]
parseArgs [] = Left "Any options given"
parseArgs ("--rule" : r : rest) = case readMaybe r of
    Just n  -> parseRest rest [RULE n]
    Nothing -> Left "The option --rule must be followed by a available value."
parseArgs (opt : _) = Left $ "The first argument must be '--rule <nombre>'"

parseRest :: [String] -> [Token] -> Either String [Token]
parseRest [] tokens = Right tokens
parseRest (opt : val : rest) tokens = case (opt, readMaybe val) of
    ("--start", Just n)  -> parseRest rest (tokens ++ [START n])
    ("--window", Just n) -> parseRest rest (tokens ++ [WINDOW n])
    ("--lines", Just n)  -> parseRest rest (tokens ++ [LINES n])
    ("--move", Just n)   -> parseRest rest (tokens ++ [MOVE n])
    _ -> Left $ "Invalid option or missing required value: " ++ opt
parseRest _ _ = Left "Syntax error: option <value>."

assignValue :: [Token] -> (Int, Int, Int, Int, Int)
assignValue tokens =
  ( extractRule tokens
  , extractStart tokens
  , extractWindow tokens
  , extractLines tokens
  , extractMove tokens
  )

extractRule :: [Token] -> Int
extractRule (RULE v : _) = v
extractRule _ = error "Option --rule is required"

extractStart :: [Token] -> Int
extractStart (START v : _) = v
extractStart (_ : xs) = extractStart xs
extractStart [] = 0

extractWindow :: [Token] -> Int
extractWindow (WINDOW v : _) = v
extractWindow (_ : xs) = extractWindow xs
extractWindow [] = 80

extractLines :: [Token] -> Int
extractLines (LINES v : _) = v
extractLines (_ : xs) = extractLines xs
extractLines [] = -1

extractMove :: [Token] -> Int
extractMove (MOVE v : _) = v
extractMove (_ : xs) = extractMove xs
extractMove [] = 0

validateRule :: Int -> IO ()
validateRule rule = do
  if rule `elem` [30, 90, 110]
  then return ()
  else exitError "Unexpected rule. The available rules are 30, 90, et 110."

parseNumber :: Maybe Int -> Maybe Int
parseNumber Nothing = Nothing
parseNumber (Just x) = Just x
