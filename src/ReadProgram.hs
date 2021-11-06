module ReadProgram (readProgramHandle) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (exitFailure)
import System.IO
import While

readLineT :: Handle -> IO Text
readLineT h = T.pack <$> hGetLine h

readProgramHandle :: Handle -> IO WhileProgram
readProgramHandle h
    | h /= stdin =
        T.pack <$> hGetContents h
            >>= let feed how l = case how l of
                        Fail a b c -> print a >> print b >> print c >> exitFailure
                        Done _ p -> pure p
                        Partial cont -> feed cont mempty -- attoparsec expects empty string if finished.
                 in feed parse
    | otherwise =
        readLineT h
            >>= let feed how l = case how l of
                        Fail a b c -> print a >> print b >> print c >> exitFailure
                        Done _ p -> pure p
                        Partial cont -> readLineT h >>= feed cont
                 in feed parse
