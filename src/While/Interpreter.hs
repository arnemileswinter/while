module While.Interpreter (interpret) where

import Control.Monad (replicateM_)
import Control.Monad.Trans.State (State, execState, get, state)
import Data.Map (Map)
import qualified Data.Map as M
import While.While

type ExecutionContext a = State Memory a

newtype Memory = Memory {unMemory::(Map Register Int)} deriving (Show, Eq)

memLookup :: Register -> ExecutionContext Int
memLookup r = get >>= \(Memory m) -> pure $ maybe 0 id $ M.lookup r m

memUpdate :: Register -> Int -> ExecutionContext ()
memUpdate r v = state (\(Memory m) -> ((), Memory $ M.insert r v m))

interpretExpr :: Expr -> ExecutionContext Int
interpretExpr (ExprConstant i) = pure i
interpretExpr (ExprRegister r) = memLookup r
interpretExpr (ExprAddition a b) = interpretExpr a >>= \a' -> interpretExpr b >>= \b' -> pure $ a' + b'
interpretExpr (ExprSubtract a b) = interpretExpr a >>= \a' -> interpretExpr b >>= \b' -> pure $ a' - b'

interpretStatement :: Statement -> ExecutionContext ()
interpretStatement (Assignment r expr) = (interpretExpr expr) >>= memUpdate r
interpretStatement (For r p) = memLookup r >>= \i -> replicateM_ i (interpretProgram p)
interpretStatement (While r p) =
    memLookup r >>= \i ->
        if i == 0
            then pure ()
            else interpretProgram p >> interpretStatement (While r p)

interpretProgram :: WhileProgram -> ExecutionContext ()
interpretProgram (Concatenation s p) = interpretStatement s >> interpretProgram p
interpretProgram (Program s) = interpretStatement s

interpret :: WhileProgram -> (Map Register Int)
interpret p = unMemory $ execState (interpretProgram p) (Memory M.empty)
