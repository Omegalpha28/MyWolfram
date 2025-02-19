module Args (parseArgs, assignValue, validateRule, Token(..), safeTail) where

import Control.Exception (throwIO)
import System.Environment
import Text.Read (readMaybe)
import ErrorHandling (invalidOptionError, handleHelpException, exitError)
import Display
import Data.Char (ord)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

data Token = RULE Int | START Int | WINDOW Int | LINES Int | MOVE Int | CHARACTER Char | BAD Int | NCURSES Int deriving (Eq, Show)

parseArgs :: [String] -> Either String [Token]
parseArgs [] = Left "Any options given"
parseArgs ("--rule" : r : rest) = case readMaybe r of
    Just n  -> parseRest rest [RULE n]
    Nothing -> Left "The option --rule must be followed by an available value."
parseArgs (opt : _) = Left $ "The first argument must be '--rule <value>'"

parseRest :: [String] -> [Token] -> Either String [Token]
parseRest [] tokens = Right tokens
parseRest (opt : val : rest) tokens
    | opt == "--c", Just c <- safeHead val = parseRest rest (tokens ++ [CHARACTER c])
    | otherwise = case readMaybe val of
        Just n -> case opt of
            "--start"  -> parseRest rest (tokens ++ [START n])
            "--window" -> parseRest rest (tokens ++ [WINDOW n])
            "--lines"  -> parseRest rest (tokens ++ [LINES n])
            "--move"   -> parseRest rest (tokens ++ [MOVE n])
            "--bad"    -> parseRest rest (tokens ++ [BAD n])
            "--vty"    -> parseRest rest (tokens ++ [NCURSES n])
            _          -> Left $ "Invalid option or missing required value: " ++ opt
        Nothing -> Left $ "Invalid option or missing required value: " ++ opt
parseRest _ _ = Left "Syntax error: option <value>."



assignValue :: [Token] -> (Int, Int, Int, Int, Int, Char, Int, Int)
assignValue tokens =
  ( extractRule tokens
  , extractStart tokens
  , extractWindow tokens
  , extractLines tokens
  , extractMove tokens
  , extractChar tokens
  , extractBad tokens
  , extractNcurses tokens
  )

extractRule :: [Token] -> Int
extractRule (RULE v : _) = v
extractRule _ = -1

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

extractChar :: [Token] -> Char
extractChar (CHARACTER v : _) = v
extractChar (_ : xs) = extractChar xs
extractChar [] = '*'

extractBad :: [Token] -> Int
extractBad (BAD v : _) = v
extractBad (_ : xs) = extractBad xs
extractBad [] = 0

extractNcurses :: [Token] -> Int
extractNcurses (NCURSES v : _) = v
extractNcurses (_ : xs) = extractNcurses xs
extractNcurses [] = 0

validateRule :: Int -> IO ()
validateRule rule = do
  if rule `elem` [30, 54, 60, 62, 90, 94, 102, 110, 122, 126, 150, 158, 182, 188, 190, 220, 222, 250]
  then return ()
  else exitError "Unexpected rule: The available rules are 30, 54, 60, 62, 90, 94, 102, 110, 122, 126, 150, 158, 182, 188, 190, 220, 222 and 250"

