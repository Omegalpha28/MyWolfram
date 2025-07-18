{-
-- EPITECH PROJECT, 2025
-- wolfram
-- File description:
-- Main
-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Display (displayAutomaton)
import Cells (generateAutomaton)
import ErrorHandling (invalidOptionError, handleHelpException, invalidNumberError)
import Args (parseArgs, assignValue, validateRule)
import Data.Char (ord)
import Secret (runBadApple)

main :: IO ()
main = do
    args <- getArgs
    if (length args < 2 || args !! 1 == "--help" || args !! 1 == "--h")
        then handleHelpException
    else do
        let result = parseArgs args
        case result of
            Left err -> putStrLn err >> exitWith (ExitFailure 84)
            Right opts -> do
                let (rule, start, window, lines, move, character, bad, ncurses) = assignValue opts
                if (lines == -1) then invalidOptionError "--lines"
                else if (window < 0) then invalidOptionError "--window"
                else if (start < 0) then invalidNumberError ("Start index is out of bounds")
                else if (ord character <= 32 || ord character >= 127) then invalidOptionError "--c"
                else if (start < 0) then invalidOptionError "--start"
                else do
                    validateRule rule
                    let newstart = start + 1
                    let newLines = lines + newstart - 1
                    automaton <- generateAutomaton rule newstart window newLines move character
                    displayAutomaton newstart window move newLines automaton ncurses
                    if (bad == 1 && ncurses == 0)
                        then do
                            putStrLn "Do you like apples?: (y/n)"
                            result <- getChar
                            putStrLn "I don't care take this apple!"
                            runBadApple character
                            exitWith ExitSuccess
                    else if (bad == 1 && ncurses /= 0)
                        then do
                            putStrLn "Turn off vty!"
                            exitWith ExitSuccess
                    else do
                        exitWith ExitSuccess