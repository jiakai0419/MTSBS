module Transformers
    ( Name
    , Exp (..)
    , Value (..)
    , Env (..)
    , eval6
    , runEval6
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

type Eval6 a = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer IO))) a

runEval6 :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env st ev = runStateT (runWriterT (runErrorT (runReaderT ev env))) st

tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)

eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do tick
                   liftIO $ print i
                   return $ IntVal i
eval6 (Var n) = do tick
                   tell [n]
                   env <- ask
                   case Map.lookup n env of
                     Nothing -> throwError $ "undefined variable: " ++ n
                     Just val -> return val
eval6 (Plus e1 e2) = do tick
                        val1 <- eval6 e1
                        val2 <- eval6 e2
                        case (val1, val2) of
                          (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                          _ -> throwError "type error in addition"
eval6 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval6 (App e1 e2) = do tick
                       val1 <- eval6 e1
                       val2 <- eval6 e2
                       case val1 of
                         FunVal env' n body -> local (const (Map.insert n val2 env')) (eval6 body)
                         _ -> throwError "type error in application"

