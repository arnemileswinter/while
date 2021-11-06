module Main where

import While
import ReadProgram (readProgramHandle)
import System.IO
import Options

data MainOptions = MainOptions { inputFile :: String } deriving Show

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "input-file" "" "The input WHILE-Program. Will read from standard input if empty."

run = putStrLn . prettyMemory . interpret

main :: IO ()
main = runCommand $ \opts _ -> do
    case inputFile opts of
        "" -> readProgramHandle stdin >>= run
        fn -> do
            h <- openFile fn ReadMode
            p <- readProgramHandle h
            putStrLn fn
            run p
    -- p <- 
    -- putStrLn $ prettyMemory $ interpret p
