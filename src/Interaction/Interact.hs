{-# LANGUAGE CPP #-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}

module Interaction.Interact
    ( InputConfig (..)
    , defaultInputConfig
    , Prompt
    , Quit
    , HistoryFile
    , genericInteract
    , inputLines
    , inputLines'
    , outputLines
    , outputLinesWithPrompt
    , outputLines'
    , outputLinesWithPrompt'
    ) where

import Prelude hiding ( interact )
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Data.Bool
import Data.Char
import Data.Maybe
import System.IO hiding (interact)
import System.IO.Unsafe
import System.Console.Haskeline

type Prompt        = String
type Quit          = String
type HistoryFile   = Maybe FilePath

data InputConfig
    = InputConfig
    { quit    :: Quit
    , history :: HistoryFile
    }

defaultInputConfig :: InputConfig
defaultInputConfig = InputConfig { quit =":q", history = Nothing }

type Dialogue a b  = [a] -> [b]

genericInteract :: IO [a] -> ([b] -> IO ()) -> ([a] -> [b]) -> IO ()
genericInteract input output translate
    = output =<< (translate <$> input)

inputLines :: IO [String]
inputLines
    = unsafeInterleaveIO
    $ do
    { minput <- runInputT defaultSettings (getInputLine "")
    ; case minput of
        Nothing   -> return []
        Just line -> (line :) <$> inputLines
    }

inputLines' :: InputConfig -> IO [String]
inputLines' config
    = unsafeInterleaveIO
    $ do
    { minput <- runInputT (defaultSettings { historyFile = config.history })
                          (getInputLine "")
    ; case minput of
        Nothing -> return []
        Just line
            | line == config.quit -> return []
            | otherwise           -> (line :) <$> inputLines' config
    }

outputLines :: [String] -> IO ()
outputLines = mapM_ putStrLn

outputLinesWithPrompt :: [String] -> IO ()
outputLinesWithPrompt ss = mapM_ putStr' $ takeWhile (/= eot) ss
    where
        putStr' str = putStr str >> hFlush stdout

outputLines' :: Chan String -> [String] -> IO ()
outputLines' chan ss = do
    { _ <- forkIO $ writeList2Chan chan (ss ++ [eot])
    ; mapM_ putStrLn . takeWhile (eot /=) =<< getChanContents chan
    }

outputLinesWithPrompt' :: Chan String -> [String] -> IO ()
outputLinesWithPrompt' chan ss = do
    { _ <- forkIO $ writeList2Chan chan (ss ++ [eot])
    ; mapM_ putStr' . takeWhile (eot /=) =<< getChanContents chan
    }
    where
        putStr' str = putStr str >> hFlush stdout

eot :: String
eot = "\EOT"
