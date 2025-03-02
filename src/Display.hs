{-
-- EPITECH PROJECT, 2025
-- wolfram
-- File description:
-- Display
-}
module Display (displayAll, displayInfinite) where

import Rules (applyRule, applyRuleG)
import System.IO (hFlush, stdout)

centerLine :: String -> Int -> String
centerLine line window =
    let width = length line
        totalPadding = max 0 ((window - width) `div` 2)
    in replicate totalPadding ' ' ++ line

applyMove :: String -> Int -> String
applyMove line move
    | move < 0 = drop (-move) line
    | move > 0 = replicate move ' ' ++ line
    | otherwise = line

formatL :: String -> Int -> Int -> String
formatL line window move =
    let centered = centerLine line window
        moved = applyMove centered move
    in moved

displayAll :: Int -> Int -> Int -> Int -> Int -> Char -> String -> IO ()
displayAll r start w l m c fL =
    mapM_ putStrLn (map (\line -> formatL line w m) (generateLines r l fL c))

displayInfinite :: Int -> Int -> Int -> Int -> Int -> Char -> String -> IO ()
displayInfinite rule start window lines move c firstLine =
    displayLinesInfinite rule 0 window move c firstLine

displayLinesInfinite :: Int -> Int -> Int -> Int -> Char -> String -> IO ()
displayLinesInfinite r count window move c currentL =
    putStrLn (formatL currentL window move) >>
    hFlush stdout >>
    displayLinesInfinite r (count + 1) window move c (applyRuleG r currentL c)


generateLines :: Int -> Int -> String -> Char -> [String]
generateLines _ 0 _ _ = []
generateLines rule n currentLine c =
    currentLine : generateLines rule (n - 1) (applyRuleG rule currentLine c) c
