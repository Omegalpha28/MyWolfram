{-
-- EPITECH PROJECT, 2025
-- wolfram
-- File description:
-- ErrorHandling
-}

module ErrorHandling (exitError, handleHelpException, invalidOptionError, invalidNumberError, unexpectedError) where

import System.Exit (exitWith, ExitCode (ExitFailure, ExitSuccess))

exitError :: String -> IO ()
exitError msg = putStrLn msg >> exitWith (ExitFailure 84)

handleHelpException :: IO ()
handleHelpException = mapM_ putStrLn helpMessages >> exitWith ExitSuccess

helpMessages :: [String]
helpMessages =
    [ "Usage: [OPTIONS]"
    , "  --help             Show useful info about the project."
    , "  --h                Alias for --help."
    , "  --rule    <value>  Define the rule used. (Rules available: 30, 54,"
    , "                     60, 62, 90, 94, 102,110, 122, 126, 150, 158, 182,"
    , "                     188, 190, 220, 222, 250)"
    , "  --start   <value>  Define the start value y. The value must be >= 0."
    , "  --lines   <value>  Set the number of lines printed."
    , "  --window  <value>  Set the size of the window. The size must be >= 0."
    , "  --move    <value>  Set the start value x."
    , "  --c       <char>   Define the character used."
    , "                     Must be a printable ASCII character."
    , "  --bad     <value>  That's a bad bonus, don't do it!"
    , "  --vty     <value>  Display with vty. (Press 'q' to stop vty)"
    ]

invalidOptionError :: String -> IO ()
invalidOptionError opt = exitError $ opt ++ ". Use --help for more info."

invalidNumberError :: String -> IO ()
invalidNumberError num = exitError $ num ++ ". Keep the value accurate."

unexpectedError :: String -> IO ()
unexpectedError errMsg = exitError $ "Unexpected error: " ++ errMsg


