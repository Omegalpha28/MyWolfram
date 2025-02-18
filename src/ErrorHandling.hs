module ErrorHandling (exitError, handleHelpException, invalidOptionError, invalidNumberError, unexpectedError) where
import System.Exit (exitWith, ExitCode(ExitFailure, ExitSuccess))

exitError :: String -> IO ()
exitError msg = putStrLn msg >> exitWith (ExitFailure 84)

handleHelpException :: IO ()
handleHelpException = do
    putStrLn "Usage: [OPTIONS]"
    putStrLn "  --help              Show useful info about the project."
    putStrLn "  --h                 Alias for --help."
    putStrLn "  --rule    <value>   Define the rule used. (Rules available : 30, 54, 60, 62, 90, 94, 102, 110, 122, 126, 150, 158, 182, 188, 190, 220, 222, 250)"
    putStrLn "  --start   <value>   Define the start value y. The value must be equal or greater than 0."
    putStrLn "  --lines   <value>   Set the number of lines printed. The number must be equal or greater than 0."
    putStrLn "  --window  <value>   Set the size of the window. The size must be equal or greater than 0."
    putStrLn "  --move    <value>   Set the start value x."
    putStrLn "  --c       <char>    Define the character used. The character must be a printable ASCII character."
    exitWith ExitSuccess

invalidOptionError :: String -> IO ()
invalidOptionError opt = exitError ("Invalid option: " ++ opt ++ ". Use --help for some informations.")

invalidNumberError :: String -> IO ()
invalidNumberError num = exitError ("Invalid number: " ++ num ++ ". Keep the value accurate.")

unexpectedError :: String -> IO ()
unexpectedError errMsg = exitError ("Unexpected error: " ++ errMsg)

