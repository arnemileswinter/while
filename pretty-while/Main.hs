module Main where

import qualified Data.Text as T
import While
import ReadProgram
import Options
import System.IO 

data MainOptions = MainOptions { inputFile :: String, prettifyFile :: Bool} deriving Show

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "input-file" "" "The input WHILE-Program. Will read from standard input if empty."
        <*> simpleOption "write-file" False "if provided, will update file contents to prettified output."

outPretty :: Handle -> WhileProgram -> IO ()
outPretty h = mapM_ ((hPutStrLn h). T.unpack) . T.lines . pretty

main :: IO ()
main = runCommand $ \opts _ -> do
    case inputFile opts of
        "" -> readProgramHandle stdin >>= outPretty stdout
        fn -> do 
            h <- openFile fn ReadMode
            p <- readProgramHandle h 
            hClose h
            if prettifyFile opts 
                then do
                   wh <- openFile fn WriteMode 
                   outPretty (wh) p
                   hClose wh
                else outPretty stdout p
    --readProgramStdIn >>= outPretty (stdout)
