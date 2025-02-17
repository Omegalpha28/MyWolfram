module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Display (displayAutomaton)
import Cells (generateAutomaton)
import ErrorHandling (invalidOptionError)
import Args (parseArgs, assignValue, validateRule)

main :: IO ()
main = do
    args <- getArgs
    let result = parseArgs args
    case result of
        Left err -> putStrLn ("Error: " ++ err) >> exitWith (ExitFailure 84)
        Right opts -> do
            let (rule, start, window, lines, move) = assignValue opts
            validateRule rule
            let newstart = start + 1
            let newLines = lines + newstart - 1
            automaton <- generateAutomaton rule newstart window newLines move
            displayAutomaton newstart window move newLines automaton
            exitWith ExitSuccess