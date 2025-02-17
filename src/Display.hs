module Display (displayAutomaton) where

displayAutomaton :: Int -> Int -> Int -> [String] -> IO ()
displayAutomaton start window lines automaton = mapM_ displayLine (take lines (drop (start - 1) automaton))
    where
        displayLine line = putStrLn (centerLine line)
        centerLine :: String -> String
        centerLine line =
            let width = length line
                totalPadding = max 0 ((window - width) `div` 2)
            in replicate totalPadding ' ' ++ line


