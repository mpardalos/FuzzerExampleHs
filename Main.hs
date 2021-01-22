{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Text.Printf
import Data.List
import System.Random
import System.Random.Stateful (uniformListM, uniformRM, Uniform(..))
import GHC.Exts
import Data.Foldable (for_)
import System.Environment (getArgs)
import System.Directory

main :: IO ()
main = do
  -- FIXME crashes if args are wrong
  [outputDir] <- getArgs
  createDirectoryIfMissing True outputDir

  for_ [1..10] $ \n -> do
    prog <- uniformIO
    let progStr = outputProgram prog
    writeFile (outputDir ++ "/testcase" ++ show n) progStr

--------- Programs ----------------------

newtype Program = Program { clauses :: [Clause] }
newtype Clause = Clause { exprs :: [Expr] }
data Expr = Y Variable | N Variable
type Variable = Int

countVars :: Program -> Int
countVars (Program clauses) =
  let allExprs :: [Expr] = concatMap exprs clauses
      vars = map exprVar allExprs
  in length (nub vars)
  where
    exprVar (Y var) = var
    exprVar (N var) = var

outputProgram prog = unlines
  [ "c testcase"
  , printf "p cnf %v %v" (countVars prog) (length (clauses prog))
  , outputProgramClauses prog
  ]
  where
    outputVar (Y var) = show var
    outputVar (N var) = "-" ++ show var

    outputProgramClauses (Program clauses) =
      let exprStrs = map (map outputVar . exprs) clauses
          clauseStrs = map unwords exprStrs
      in unlines clauseStrs

-- For use with -XOverloadedLists

instance IsList Program where
  type Item Program = Clause

  fromList = Program
  toList = clauses

instance IsList Clause where
  type Item Clause = Expr

  fromList = Clause
  toList = exprs

--------- Randomising ----------------------

uniformIO :: Uniform a => IO a
uniformIO = getStdRandom uniform

instance Uniform Program where
  uniformM gen = do
    size <- uniformRM (2, 5) gen
    Program <$> uniformListM size gen

instance Uniform Clause where
  uniformM gen = do
    size <- uniformRM (2, 5) gen
    Clause <$> uniformListM size gen

instance Uniform Expr where
  uniformM gen = uniformM gen >>= \case
    True -> Y <$> uniformRM (0, 10) gen
    False -> N <$> uniformRM (0, 10) gen
