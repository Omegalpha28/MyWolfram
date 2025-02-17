module Cells (generateAutomaton) where

import Args (assignValue, validateRule, Token(..))
import Rules (applyRule)
import ErrorHandling (exitError)

generateAutomaton :: Int -> Int -> Int -> Int -> Int -> IO [String]
generateAutomaton rule start window lines move = do
    validateRule rule
    let startPosition = start
    let adjustedWindow = max window (2 * lines)
    let padding = replicate window ' '
    let initialState = padding ++ ['*'] ++ padding
    automaton <- runAutomaton rule startPosition initialState lines move
    return automaton

infiniteLeft :: Int -> [Char]
infiniteLeft n = replicate n ' '

infiniteRight :: Int -> [Char]
infiniteRight n = replicate n ' '

runAutomaton :: Int -> Int -> [Char] -> Int -> Int -> IO [String]
runAutomaton _ _ _ 0 _ = return []
runAutomaton rule start state lines move
    | start < 0 = error "Start index is out of bounds"
    | otherwise = do
        let extendedState = replicate move ' ' ++ state ++ replicate move ' ' -- Ajouter du padding symétrique
        let nextState = applyRule rule extendedState
        nextAutomaton <- runAutomaton rule start nextState (lines - 1) move
        return (extendedState : nextAutomaton)


displayAutomaton :: [String] -> IO ()
displayAutomaton automaton = mapM_ putStrLn automaton
