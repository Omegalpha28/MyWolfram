{-
-- EPITECH PROJECT, 2025
-- wolfram
-- File description:
-- Display
-}

module Display (displayAutomaton) where

import qualified Graphics.Vty as Vty
import Graphics.Vty.Platform.Unix (mkVty)
import Control.Monad (when)
import System.Exit (exitSuccess)

displayAutomaton :: Int -> Int -> Int -> Int -> [String] -> Int -> IO ()
displayAutomaton start window move lines automaton ncurses = case ncurses of
    0 -> displayTerminal start window move lines automaton
    _ -> displayVty start window move lines automaton

displayVty :: Int -> Int -> Int -> Int -> [String] -> IO ()
displayVty st window move line automat = do
    vty <- mkVty Vty.defaultConfig
    Vty.hideCursor (Vty.outputIface vty)
    let disL = map (Vty.string Vty.defAttr) (take line (drop (st - 1) automat))
    let image = Vty.picForImage (Vty.vertCat disL)
    Vty.update vty image
    handleEvent vty

displayTerminal :: Int -> Int -> Int -> Int -> [String] -> IO ()
displayTerminal start window move lines automaton =
    mapM_ displayLine (take lines (drop (start - 1) automaton))
    where
        displayLine line = putStrLn (centerLine line)
        centerLine line =
            let width = length line
                totalPadding = max 0 ((window - width) `div` 2)
            in replicate totalPadding ' ' ++ line

handleEvent :: Vty.Vty -> IO ()
handleEvent vty = do
    event <- Vty.nextEvent vty
    case event of
        Vty.EvKey (Vty.KChar 'q') [] -> Vty.shutdown vty >> exitSuccess
        _ -> handleEvent vty
