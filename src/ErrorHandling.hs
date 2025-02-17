module ErrorHandling (exitError, handleHelpException, invalidOptionError, invalidNumberError, unexpectedError) where
import System.Exit (exitWith, ExitCode(ExitFailure))

exitError :: String -> IO ()
exitError msg = putStrLn msg >> exitWith (ExitFailure 84)

handleHelpException :: IO ()
handleHelpException = do
    putStrLn "Usage: [OPTIONS]"
    putStrLn "  --help      Show useful info about the project."
    putStrLn "  --h         Alias for --help."
    putStrLn "  --rule      Define the rule used. (Rule aviable : 30, 90, 110)"
    putStrLn "  --start     Define the start value."
    putStrLn "  --lines     Set the number of lines"
    putStrLn "  --window    Set the size of the window."
    exitWith (ExitFailure 0)

invalidOptionError :: String -> IO ()
invalidOptionError opt = exitError ("Invalid option: " ++ opt ++ ". Use --help for some informations.")

invalidNumberError :: String -> IO ()
invalidNumberError num = exitError ("Invalid number: " ++ num ++ ". Keep the value accurate.")

unexpectedError :: String -> IO ()
unexpectedError errMsg = exitError ("Unexpected error: " ++ errMsg)

