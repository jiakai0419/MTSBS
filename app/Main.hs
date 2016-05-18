module Main where

import Transformers
import qualified Data.Map as Map

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

main :: IO ()
main = putStrLn . show . runEval1 . eval1 Map.empty $ exampleExp
