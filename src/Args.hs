{-
-- EPITECH PROJECT, 2025
-- wolfram
-- File description:
-- Args
-}
module Args (parseArgs, assignValue, validateRule, Token(..), safeTail) where

import Control.Exception (throwIO)
import System.Environment
import Text.Read (readMaybe)
import ErrorHandling (invalidOptionError, handleHelpException, exitError)
import Data.Char (ord)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

data Token = RULE Int | START Int | WINDOW Int | LINES Int | MOVE Int | CHARACTER Char deriving (Eq, Show)

parseArgs :: [String] -> Either String [Token]
parseArgs [] = Left "Any options given"
parseArgs ("--rule" : r : rest)
  | Just n <- readMaybe r = parseRest rest [RULE n]
  | otherwise = Left "--rule must be followed by an available value."
parseArgs (opt : _) = Left $ "The first argument must be '--rule <value>'"

parseRest :: [String] -> [Token] -> Either String [Token]
parseRest [] tokens = Right tokens
parseRest ("--c" : val : rest) tokens
  | not (null val) = parseRest rest (tokens ++ [CHARACTER (head val)])
  | otherwise = Left "--c must be followed by a character."
parseRest (opt : val : rest) tokens =
  case readMaybe val of
    Just n -> parseOption opt n rest tokens
    Nothing -> Left $ "Invalid option or missing required value: " ++ opt
parseRest [opt] _ = Left $ "Option " ++ opt ++ " requires a value."

parseOption :: String -> Int -> [String] -> [Token] -> Either String [Token]
parseOption opt n rest tokens = case opt of
  "--start" -> parseRest rest (tokens ++ [START n])
  "--window" -> parseRest rest (tokens ++ [WINDOW n])
  "--lines" -> parseRest rest (tokens ++ [LINES n])
  "--move" -> parseRest rest (tokens ++ [MOVE n])
  _ -> Left $ "Invalid option: " ++ opt

assignValue :: [Token] -> (Int, Int, Int, Int, Int, Char)
assignValue tokens =
  ( extractRule tokens
  , extractStart tokens
  , extractWindow tokens
  , extractLines tokens
  , extractMove tokens
  , extractChar tokens
  )

extractRule :: [Token] -> Int
extractRule [] = -1
extractRule (RULE v : _) = v
extractRule (_ : xs) = extractRule xs

extractStart :: [Token] -> Int
extractStart [] = 0
extractStart (START v : _) = v
extractStart (_ : xs) = extractStart xs

extractWindow :: [Token] -> Int
extractWindow [] = 80
extractWindow (WINDOW v : _) = v
extractWindow (_ : xs) = extractWindow xs

extractLines :: [Token] -> Int
extractLines [] = -1
extractLines (LINES v : _) = v
extractLines (_ : xs) = extractLines xs

extractMove :: [Token] -> Int
extractMove [] = 0
extractMove (MOVE v : _) = v
extractMove (_ : xs) = extractMove xs

extractChar :: [Token] -> Char
extractChar [] = '*'
extractChar (CHARACTER v : _) = v
extractChar (_ : xs) = extractChar xs

validateRule :: Int -> IO ()
validateRule rule =
  if rule `elem` [30, 54, 60, 62, 90, 94, 102, 110, 122, 126, 150, 158,
                 182, 188, 190, 220, 222, 250]
  then return ()
  else exitError "Unexpected rule: try 30, 54 or 60 for example"