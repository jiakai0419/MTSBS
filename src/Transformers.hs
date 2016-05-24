module Transformers
    ( Name
    , Exp (..)
    , Value (..)
    , Env (..)
    , eval3
    , runEval3
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

type Eval4 a = ReaderT Env (ErrorT String (StateT Integer Identity)) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity $ runErrorT $ runReaderT ev env

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do env <- ask
                   case Map.lookup n env of
                     Nothing -> throwError $ "undefined variable: " ++ n
                     Just val -> return val
eval3 (Plus e1 e2) = do val1 <- eval3 e1
                        val2 <- eval3 e2
                        case (val1, val2) of
                          (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                          _ -> throwError "type error in addition"
eval3 (Abs n e) = do env <- ask
                     return $ FunVal env n e
eval3 (App e1 e2) = do val1 <- eval3 e1
                       val2 <- eval3 e2
                       case val1 of
                         FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body)
                         _ -> throwError "type error in application"

