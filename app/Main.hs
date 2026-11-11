{-# LANGUAGE QualifiedDo #-}

module Main (main) where

import           Board.Check                 (runBoardCheck)
import           Board.Session               (runBoardTest)
import           Config                      (defaultConfigPath)
import           Keyboard.LED.HardwareTest  (printResult, runHardwareTest,
                                             standardFrames, walkingFrames)
import           Keyboard.Monitor            (runKeyMonitor)
import           System.Environment          (getArgs)
import           System.IO                   (BufferMode(..), hSetBuffering,
                                             stderr, stdout)

printUsage :: IO ()
printUsage = putStrLn
  "usage: kvmSwitch --key-monitor [CONFIG] | --board-test [CONFIG] | --board-check [CONFIG] | --led-self-test | --led-walking-test"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  putStrLn "[kvmSwitch] starting…"
  args <- getArgs
  case args of
    ["--key-monitor"] -> defaultConfigPath >>= runKeyMonitor
    ["--key-monitor", configFile] -> runKeyMonitor configFile
    ["--board-test"] -> defaultConfigPath >>= runBoardTest
    ["--board-test", configFile] -> runBoardTest configFile
    ["--board-check"] -> defaultConfigPath >>= runBoardCheck
    ["--board-check", configFile] -> runBoardCheck configFile
    ["--led-self-test"] -> do
      result <- runHardwareTest standardFrames
      printResult result
    ["--led-walking-test"] -> do
      result <- runHardwareTest walkingFrames
      printResult result
    _ -> printUsage
