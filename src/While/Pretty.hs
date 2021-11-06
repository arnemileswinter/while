{-# LANGUAGE OverloadedStrings #-}
module While.Pretty (pretty, prettyMemory) where

import While.While

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T

tshow :: Show a => a -> Text
tshow = T.pack . show

indent :: Text -> Text
indent t = T.unlines $ map (("    ") <>) $ T.lines t

prettyRegister :: Register -> Text
prettyRegister (Register i) = "x" <> tshow i

prettyExpr :: Expr -> Text
prettyExpr (ExprRegister r) = prettyRegister r
prettyExpr (ExprConstant i) = tshow i
prettyExpr (ExprAddition e1 e2) = prettyExpr e1 <> " + " <> prettyExpr e2
prettyExpr (ExprSubtract e1 e2) = prettyExpr e1 <> " - " <> prettyExpr e2

prettyStatement :: Statement -> Text
prettyStatement (Assignment r e) = prettyRegister r <> " = " <> prettyExpr e
prettyStatement (For r p) =
    "for " <> prettyRegister r <> " do\n"
        <> indent (pretty p)
        <> "end"
prettyStatement (While r p) =
    "while " <> prettyRegister r <> " != 0 do\n"
        <> indent (pretty p)
        <> "end"

pretty :: WhileProgram -> Text
pretty (Concatenation s p) = prettyStatement s <> ";\n" <> pretty p
pretty (Program s) = prettyStatement s

prettyMemory :: Map Register Int -> String
prettyMemory mem = let
        out = ("Register", "Value") : (map (\((Register r), v) -> (show r, show v)) $ M.toList mem)
        (maxL, maxR) = foldr (\(a,b) (a',b') -> (max a a', max b b')) (0,0) $ map (\(a,b) -> (length a, length b)) out
        (header:vals) = map (pipesAround maxL maxR) out
        separator = "|" ++ (replicate (2 + maxL) '-') ++ "|" ++ (replicate (2 + maxR) '-') ++ "|"
    in unlines $ header:separator:vals
    where
        pipesAround :: Int -> Int -> (String,String) -> String
        pipesAround l r (a,b) = "| " <> (a <> (replicate (l - length a) ' ')) <> " | " <> (b <> (replicate (r - length b) ' '))  <> " |"

