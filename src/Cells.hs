module Cells (generateAutomaton) where

import Args (assignValue, validateRule, Token(..))
import Rules (applyRule)
import ErrorHandling (exitError)

generateAutomaton :: Int -> Int -> Int -> Int -> Int -> Char -> IO [String]
generateAutomaton rule start window lines move character = do
    validateRule rule
    let startPosition = start
    let adjustedWindow = max window (2 * lines)
    let padding = replicate window ' '
    let initialState = padding ++ [character] ++ padding
    automaton <- runAutomaton rule startPosition initialState lines move character
    return automaton

infiniteLeft :: Int -> [Char]
infiniteLeft n = replicate n ' '

infiniteRight :: Int -> [Char]
infiniteRight n = replicate n ' '

runAutomaton :: Int -> Int -> [Char] -> Int -> Int -> Char -> IO [String]
runAutomaton _ _ _ 0 _ _ = return []
runAutomaton rule start state lines move character
    | start < 0 = error "Start index is out of bounds"
    | move == 0 = do
        let extendedState = replicate move ' ' ++ state ++ replicate move  ' '
        let nextState = applyRule rule extendedState character
        nextAutomaton <- runAutomaton rule start nextState (lines - 1) move character
        return (extendedState : nextAutomaton)
    | otherwise = do
        let extendedState = replicate move ' ' ++ state ++ replicate move ' '
        let nextState = applyRule rule extendedState character
        nextAutomaton <- runAutomaton rule start nextState (lines - 1) move character
        return (extendedState : nextAutomaton)


displayAutomaton :: [String] -> IO ()
displayAutomaton automaton = mapM_ putStrLn automaton
