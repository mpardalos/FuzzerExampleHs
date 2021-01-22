module Main where

import Text.Printf
import Data.List

exampleProg :: Program
exampleProg = Program [[Y 1, N 3], [N 1, Y 2], [Y 1]]

main :: IO ()
main = putStrLn (outputProgram exampleProg)

--------- Programs ----------------------

newtype Program = Program { clauses :: [Clause] }
type Clause = [Expr]
data Expr
  = Y Variable
  | N Variable
type Variable = Int

countVars :: Program -> Int
countVars (Program clauses) =
  let exprs = concat clauses
      vars = map exprVar exprs
  in length (nub vars)
  where
    exprVar (Y var) = var
    exprVar (N var) = var

outputVar (Y var) = show var
outputVar (N var) = "-" ++ show var

outputProgram prog = unlines
  [ "c testcase"
  , printf "p cnf %v %v" (countVars prog) (length (clauses prog))
  , outputProgramClauses prog
  ]

outputProgramClauses (Program clauses) =
    let exprStrs = map (map outputVar) clauses
        clauseStrs = map unwords exprStrs
    in unlines clauseStrs
