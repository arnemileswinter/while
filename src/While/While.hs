module While.While where

newtype Register
    = Register Int
    deriving (Show, Eq, Ord)

data Expr
    = ExprRegister Register
    | ExprConstant Int
    | ExprAddition Expr Expr
    | ExprSubtract Expr Expr
    deriving (Show, Eq)

data Statement
    = Assignment Register Expr
    | For Register WhileProgram
    | While Register WhileProgram
    deriving (Eq, Show)

data WhileProgram = Concatenation Statement WhileProgram
                  | Program Statement
    deriving (Eq, Show)
