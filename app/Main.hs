module Main where

import Transformers
import qualified Data.Map as Map

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
exampleExp1 = Plus (Lit 1) (Abs "x" (Var "x"))
exampleExp2 = Var "x"

main :: IO ()
main = do
  result <- runEval6 Map.empty 0 . eval6 $ exampleExp
  print result
