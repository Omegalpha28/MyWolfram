{-
-- EPITECH PROJECT, 2025
-- wolfram
-- File description:
-- Display
-}

module Display (displayAutomaton) where
import Control.Monad (when)
import System.Exit (exitSuccess)

displayAutomaton :: Int -> Int -> Int -> Int -> [String] -> IO ()
displayAutomaton start window move lines automaton =
    displayTerminal start window move lines automaton

displayTerminal :: Int -> Int -> Int -> Int -> [String] -> IO ()
displayTerminal start window move lines automaton =
    mapM_ displayLine (take lines (drop (start - 1) automaton))
    where
        displayLine line = putStrLn (centerLine line)
        centerLine line =
            let width = length line
                totalPadding = max 0 ((window - width) `div` 2)
            in replicate totalPadding ' ' ++ line

