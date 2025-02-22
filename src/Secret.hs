{- 
-- EPITECH PROJECT, 2025
-- wolfram
-- File description:
-- Secret
-}

module Secret (runBadApple) where

import System.Process (callCommand, createProcess, proc)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing, removeDirectoryRecursive, getDirectoryContents)
import System.IO (hFlush, stdout)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Monad (when, forM_)
import Control.Concurrent (threadDelay)
import qualified System.FilePath as FP
import Control.Exception (try, SomeException)

asciiChars :: String
asciiChars = "@#S%?*+;:, "

frameInterval :: Double
frameInterval = 1.0 / 30

frameSize :: Int
frameSize = 150

pixelsToAscii :: Char -> String -> String
pixelsToAscii character pixels = concatMap (const [character]) pixels

asciiGenerator :: Char -> String -> Int -> IO ()
asciiGenerator character imageFrame frameCount =
    writeTextFile ("bonus/TextFiles/bad_apple" ++ show frameCount ++ ".txt")
        (pixelsToAscii character imageFrame)

playAudio :: IO ()
playAudio =
    putStrLn "Playing audio..." >>
    createProcess (proc "mpv" ["--no-video", "bonus/bad_apple_audio.mp3"]) >>
    return ()

extractFrames :: FilePath -> IO ()
extractFrames videoPath =
    putStrLn "Simulating frame extraction..." >>
    let outputDir = "ExtractedFrames" in
    createDirectoryIfMissing True outputDir >>
    mapM_ (\i -> writeFile (outputDir ++ "/BadApple_" ++ show i ++ ".jpg") "Frame content") [1..10] >>
    putStrLn "Frame extraction complete."

checkFrames :: IO ()
checkFrames =
    let path = "ExtractedFrames" in
    doesDirectoryExist path >>= \exists ->
    if exists
        then putStrLn "Frames found, proceeding to next step."
        else extractFrames "BadApple.mp4"


writeTextFile :: FilePath -> String -> IO ()
writeTextFile path content = writeFile path content

checkTxt :: Char -> IO ()
checkTxt char = do
    let sourceDir = "bonus/TextFiles"
    let targetDir = "TextFiles"

    exists <- doesDirectoryExist sourceDir
    if exists then do
        putStrLn ".txt files found, proceeding to copy and modify."
        createDirectoryIfMissing True targetDir
        files <- getDirectoryContents sourceDir
        let txtFiles = filter (\f -> FP.takeExtension f == ".txt") files
        forM_ txtFiles $ \file -> do
            let sourceFile = sourceDir FP.</> file
            let targetFile = targetDir FP.</> file
            content <- readFile sourceFile
            let modifiedContent = map (replaceCharacter char) content
            writeFile targetFile modifiedContent
    else
        putStrLn ".txt files not found, creating .txt files..." >>
        mapM_ (\i -> asciiGenerator char "some_character" i) [1..10] >>
        putStrLn ".txt files created, proceeding to animation."

replaceCharacter :: Char -> Char -> Char
replaceCharacter newChar _ = newChar

playVideo :: IO ()
playVideo =
    let frameCount = 6571 in
    mapM_ (\frameNum ->
        getCurrentTime >>= \startTime ->
        let fileName ="bonus/TextFiles/bad_apple" ++ show frameNum ++ ".txt" in
        readFile fileName >>= \content ->
        putStrLn content >>
        getCurrentTime >>= \currentTime ->
        let computeDelay = diffUTCTime currentTime startTime in
        let delayDuration = frameInterval - realToFrac computeDelay in
        putStrLn ("Computed delay: " ++ show computeDelay) >>
        threadDelay (round (delayDuration * 1000000))) [1..frameCount]

deleteAssets :: IO ()
deleteAssets =
    putStrLn "Delete assets? (Y/n): " >>
    getLine >>= \userInput ->
    when (userInput `elem` ["Y", "y"]) $
        removeDirectoryRecursive "ExtractedFrames" >>
        putStrLn "Assets deleted."


runBadApple :: Char -> IO ()
runBadApple character =
    checkFrames >>
    checkTxt character >>
    playAudio >>
    playVideo >>
    deleteAssets >>
    putStrLn "Stopping audio..." >>
    callCommand "pkill mpv"
