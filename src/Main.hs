{-
-- EPITECH PROJECT, 2025
-- wolfram
-- File description:
-- Main
-}
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Args (parseArgs, assignValue, validateRule)
import Rules (applyRule)
import Display (displayAll, displayInfinite)
import ErrorHandling (exitError, handleHelpException, invalidOptionError, invalidNumberError, unexpectedError, validateOptions)

handleError :: [String] -> Either String [String]
handleError args
    | length args < 2 = Left "Error: Not enough arguments"
    | head args == "--help" || head args == "--h" = Left "Displaying help..."
    | otherwise = case parseArgs args of
        Left err -> Left err
        Right opts -> Right (map show opts)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Error: Not enough args!" >> exitWith (ExitFailure 84)
        ("--help":_) -> handleHelpException
        ("--h":_) -> handleHelpException
        _ -> case parseArgs args of
            Left err -> putStrLn err >> exitWith (ExitFailure 84)
            Right opts' -> do
                let (r, start, win, line, move, c) = assignValue opts'
                validationResult <- validateOptions line win start c
                case validationResult of
                    Just err -> putStrLn err >> exitWith (ExitFailure 84)
                    Nothing -> validateRule r >>
                        let initS = replicate (win `div` 2) ' ' ++ [c] ++ replicate (win `div` 2) ' '
                            fL = applyRule r start initS c
                        in if line < 0
                            then displayInfinite r start win (-line) move c fL
                            else displayAll r start win line move c fL
                        >> exitWith ExitSuccess

