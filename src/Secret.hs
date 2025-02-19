module Secret (runBadApple) where

import System.Process (callCommand, waitForProcess, createProcess, proc)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing, removeDirectoryRecursive, getDirectoryContents)
import System.IO (hFlush, stdout)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Monad (when, forM_)
import Control.Concurrent (threadDelay)
import qualified Data.List as L
import qualified System.FilePath as FP
import Control.Exception (try, SomeException)

asciiChars :: String
asciiChars = "@#S%?*+;:, "

frameInterval :: Double
frameInterval = 1.0 / 30

frameSize :: Int
frameSize = 150

pixelsToAscii :: Char -> String -> String
pixelsToAscii character pixels = concatMap (\p -> [character]) pixels

asciiGenerator :: Char -> String -> Int -> IO ()
asciiGenerator character imageFrame frameCount = do
    let asciiImage = pixelsToAscii character imageFrame
    let fileName = "Bonus/TextFiles/bad_apple" ++ show frameCount ++ ".txt"
    writeTextFile fileName asciiImage
    return ()


playAudio :: IO ()
playAudio = do
    putStrLn "Playing audio..."
    _ <- createProcess (proc "mpv" ["--no-video", "Bonus/bad_apple_audio.mp3"])
    return ()

extractFrames :: FilePath -> IO ()
extractFrames videoPath = do
    putStrLn "Simulating frame extraction..."
    let outputDir = "ExtractedFrames"
    createDirectoryIfMissing True outputDir
    mapM_ (\i -> writeFile (outputDir ++ "/BadApple_" ++ show i ++ ".jpg") "Frame content") [1..10]
    putStrLn "Frame extraction complete."

checkFrames :: IO ()
checkFrames = do
    let path = "ExtractedFrames"
    exists <- doesDirectoryExist path
    if exists
        then putStrLn "Frames found, proceeding to next step."
        else do
            putStrLn "Frames not found, extracting frames..."
            extractFrames "BadApple.mp4"

writeTextFile :: FilePath -> String -> IO ()
writeTextFile path content = writeFile path content

checkTxt :: Char -> IO ()
checkTxt char = do
    let sourceDir = "Bonus/TextFiles"
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
    else do
        putStrLn ".txt files not found, creating .txt files..."
        mapM_ (\i -> asciiGenerator char "some_character" i) [1..10]
        putStrLn ".txt files created, proceeding to animation."

replaceCharacter :: Char -> Char -> Char
replaceCharacter newChar _ = newChar

playVideo :: IO ()
playVideo = do
    let frameCount = 6571
    mapM_ (\frameNumber -> do
        startTime <- getCurrentTime
        let fileName = "Bonus/TextFiles/bad_apple" ++ show frameNumber ++ ".txt"
        content <- readFile fileName
        putStrLn content
        currentTime <- getCurrentTime
        let computeDelay = diffUTCTime currentTime startTime
        let delayDuration = frameInterval - realToFrac computeDelay
        putStrLn ("Computed delay: " ++ show computeDelay)
        threadDelay (round (delayDuration * 1000000)))
        [1..frameCount]

deleteAssets :: IO ()
deleteAssets = do
    putStrLn "Delete assets? (Y/n): "
    userInput <- getLine
    when (userInput `elem` ["Y", "y"]) $ do
        removeDirectoryRecursive "ExtractedFrames"
        putStrLn "Assets deleted."

runBadApple :: Char -> IO ()
runBadApple character = do
    checkFrames
    checkTxt character
    playAudio
    playVideo
    deleteAssets
    putStrLn "Stopping audio..."
    callCommand "pkill mpv"
