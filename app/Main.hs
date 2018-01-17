-- TODO Replace all this with an actual application (instead of test programs).
module Main where

import Control.Monad.Except (runExceptT)

import qualified Lihsp.AST as AST (Expression)
import Lihsp.AST (ToHaskell(toHaskell))

import Lihsp.Error (Error)
import Lihsp.Macros (expandMacros, extractMacroDefinitions)
import Lihsp.Normalize (normalizeExpression, normalizeStatement)
import qualified Lihsp.Parse as Parse (Expression)
import Lihsp.Parse (parseProgram)
import Lihsp.Parse.AST (toLihsp)

main :: IO ()
main = macroProgram

fromRight :: Either Error b -> b
fromRight = either (error . show) id

parse :: String -> [Parse.Expression]
parse = fromRight . parseProgram

normalize :: Parse.Expression -> AST.Expression
normalize = fromRight . normalizeExpression

macroProgram :: IO ()
macroProgram =
  runExceptT (traverse (expandMacros macroDefinitions) parsed) >>=
  putStrLn . unlines . map toLihsp . fromRight
  where
    macroDefinitions =
      extractMacroDefinitions $ map (fromRight . normalizeStatement) parsed
    macroSource =
      "(defmacro y ((_) (return (SExpression (: (Literal-int 3) (mempty))))))   (defmacro x ((_) (return (SExpression (: (Literal-int 1) (: (Literal-int 2) (y)))))))"
    programSource = "(+ 1 (x))"
    parsed = parse $ macroSource ++ programSource

quoteProgram :: IO ()
quoteProgram =
  putStrLn $
  toHaskell $
  fromRight $
  normalizeExpression $ head $ parse "(quote (quote ((quote 1 2 3) 2 3)))"
