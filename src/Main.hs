module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Display (displayAutomaton)
import Cells (generateAutomaton)
import ErrorHandling (invalidOptionError, handleHelpException)
import Args (parseArgs, assignValue, validateRule)
import Data.Char (ord)
import Secret (runBadApple)

main :: IO ()
main = do
    args <- getArgs
    if (length args < 2 || args !! 1 == "--help" || args !! 1 == "--h") then handleHelpException
    else do
        let result = parseArgs args
        case result of
            Left err -> putStrLn ("Error: " ++ err) >> exitWith (ExitFailure 84)
            Right opts -> do
                let (rule, start, window, lines, move, character, bad, ncurses) = assignValue opts
                if (lines == -1) then invalidOptionError "--lines"
                else if (window < 0) then invalidOptionError "--window"
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
                            putStrLn "oh, it seems you’re a bad person, do you like apples?: (y/n)"
                            result <- getChar
                            putStrLn "I don't care take this apple!"
                            runBadApple character
                            exitWith ExitSuccess
                    else if (bad == 1 && ncurses /= 0)
                        then do
                            putStrLn "Hehe, I refuse to give you my knowledge with this cursed shit activate! Turn off ncurses!"
                            exitWith ExitSuccess
                    else do
                        exitWith ExitSuccess