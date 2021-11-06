{-# LANGUAGE OverloadedStrings #-}

module While.Parser (parse', parse, module Data.Attoparsec.Text) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (IResult(..))
import qualified Data.Attoparsec.Text as AP
import Data.Text (Text)
import While.While

-- | alias, short than skipSpace. underscore denotes optionality of whitespace.
ws_ = AP.skipSpace
char = AP.char
decimal = AP.decimal
string = AP.string
(<?>) = (AP.<?>)

type Parser = AP.Parser

-- | parses a register statement.  register :: Parser Register
register :: Parser Register
register = char 'x' *> ((Register) <$> decimal) <?> "register"

-- | parses a mathematical expression involving constants and/or registers.
expr, exprConst, exprReg, exprAdd, exprSub :: Parser Expr
expr = (exprSub <|> exprAdd <|> exprReg <|> exprConst) <?> "expression"
exprConst = (ExprConstant <$> decimal) <?> "constant"
exprReg = (ExprRegister <$> register) <?> "register expression"
exprAdd =
    ( do
        a <- (exprConst <|> exprReg) <* ws_ <* char '+'
        b <- ws_ *> (exprConst <|> exprReg)
        pure $ ExprAddition a b
    )
        <?> "addition"
exprSub =
    ( do
        a <- (exprConst <|> exprReg) <* ws_ <* char '-'
        b <- ws_ *> (exprConst <|> exprReg)
        pure $ ExprSubtract a b
    )
        <?> "subtraction"

-- | assigns a register to an expression
assign :: Parser Statement
assign =
    ( do
        r <- (register <* ws_ <* char '=') <?> "left hand side"
        e <- (ws_ *> expr <* ws_) <?> "right hand side"
        pure $ Assignment r e
    )
        <?> "assignment"

-- | the `do` common start for while and for loops.
blockdo, end :: Parser ()
blockdo = (() <$ string "do") <?> "do"

-- | the common end for while and for loops.
end = (() <$ string "end") <?> "end"

for :: Parser Statement
for =
    ( do
        r <- (string "for" *> ws_ *> register <* ws_ <* blockdo) <?> "for head"
        p <- program <?> "for body"
        end <?> "for end"
        pure $ For r p
    )
        <?> "for"

while :: Parser Statement
while =
    ( do
        r <- (string "while" *> ws_ *> register <* ws_ <* string "!=" <* ws_ <* "0" <* ws_ <* blockdo) <?> "while head"
        p <- program <?> "while body"
        end <?> "while end"
        pure $ While r p
    )
        <?> "while"

statement :: Parser Statement
statement = ws_ *> (assign <|> while <|> for) <* ws_

concatenation :: Parser WhileProgram
concatenation =
    ( do
        p1 <- statement <* char ';'
        p2 <- program
        pure $ Concatenation p1 p2
    )
        <?> "concatenation"

program :: Parser WhileProgram
program = (concatenation <|> (Program <$> statement))

source :: Parser WhileProgram
source = program <* (AP.endOfInput <?> "program declaration")

-- | parse a while program.
parse' :: Text -> Either String WhileProgram
parse' = AP.parseOnly source

parse :: Text -> IResult Text WhileProgram
parse = AP.parse source
