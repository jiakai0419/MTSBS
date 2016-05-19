module Transformers
    ( Name
    , Exp (..)
    , Value (..)
    , Env (..)
    , eval2b
    , runEval2
    ) where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name = String

data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)

data Value = IntVal Integer
           | FunVal Env Name Exp
           deriving (Show)

type Env = Map.Map Name Value

type Eval2 a = ErrorT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 = runIdentity . runErrorT

eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i) = return $ IntVal i
eval2b env (Var n) = maybe (fail $ "undefined variable: " ++ n) return $ Map.lookup n env
eval2b env (Plus e1 e2) = do v1 <- eval2b env e1
                             v2 <- eval2b env e2
                             case (v1, v2) of
                               (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                               _ -> throwError "type error"
eval2b env (Abs n e) = return $ FunVal env n e
eval2b env (App e1 e2) = do val1 <- eval2b env e1
                            val2 <- eval2b env e2
                            case val1 of
                              FunVal env' n body -> eval2b (Map.insert n val2 env') body
                              _ -> throwError "type error"

