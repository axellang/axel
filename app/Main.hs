{-# LANGUAGE PartialTypeSignatures #-}

-- TODO Replace all this with an actual application (instead of test programs).
module Main where

import Control.Monad.Except (runExceptT)

import qualified Lihsp.AST as AST (Expression)
import Lihsp.AST (ToHaskell(toHaskell))

import Lihsp.Error (Error(MacroError))
import Lihsp.Macros (expandMacros, extractMacroDefinitions)
import Lihsp.Normalize (normalizeExpression, normalizeStatement)
import qualified Lihsp.Parse as Parse (Expression)
import Lihsp.Parse (parseProgram)

main :: IO ()
main = do
  result <-
    macroProgram $
    parse
      "(defmacro y ((z) (return (: (Literal-int z) (mempty)))))   (defmacro x ((_) (return (SExpression (: (Literal-int 1) (: (Literal-int 2) (y 4)))))))   (= main (IO Unit) (() (+ 1 (x 1))))"
  result2 <- macroProgram result
  putStrLn $ unlines $ map show result2

fromRight :: Either Error b -> b
fromRight x =
  case x of
    Right result -> result
    Left (MacroError err) -> error err
    Left err -> error $ show err

parse :: String -> [Parse.Expression]
parse = fromRight . parseProgram

normalize :: Parse.Expression -> AST.Expression
normalize = fromRight . normalizeExpression

macroProgram :: [Parse.Expression] -> IO [Parse.Expression]
macroProgram exprs =
  fromRight <$> runExceptT (traverse (expandMacros macroDefinitions) exprs)
  where
    macroDefinitions =
      extractMacroDefinitions $ map (fromRight . normalizeStatement) exprs

quoteProgram :: IO ()
quoteProgram =
  putStrLn $
  toHaskell $
  fromRight $
  normalizeExpression $ head $ parse "(quote (quote ((quote 1 2 3) 2 3)))"
