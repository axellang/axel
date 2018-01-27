{-# LANGUAGE FlexibleContexts #-}

-- TODO Replace all this with an actual application (instead of test programs).
module Main where

import Control.Lens.Operators ((%~))
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)

import Data.Semigroup ((<>))

import qualified Lihsp.AST as AST (Expression)
import Lihsp.AST
  ( Statement(SMacroDefinition, STopLevel)
  , ToHaskell(toHaskell)
  , TopLevel
  , statements
  )
import Lihsp.Error (Error(MacroError))
import Lihsp.Macros (expandMacros, extractMacroDefinitions)
import qualified Lihsp.Normalize as Normalize
  ( normalizeExpression
  , normalizeStatement
  )
import qualified Lihsp.Parse as Parse (Expression)
import Lihsp.Parse (parseSource)
import Lihsp.Utils.Monad (exhaustM)

main :: IO ()
main = do
  result <- runExceptT $ exhaustivelyExpandMacros $ parse testSource
  case result of
    Left err -> putStrLn $ "ERROR: " <> show err
    Right expr ->
      putStrLn $ toHaskell $ stripMacroDefinitions $ normalizeProgram expr
  return ()

fromRight :: Either Error b -> b
fromRight x =
  case x of
    Right result -> result
    Left (MacroError err) -> error err
    Left err -> error $ show err

parse :: String -> Parse.Expression
parse = fromRight . parseSource

normalizeExpr :: Parse.Expression -> AST.Expression
normalizeExpr = fromRight . Normalize.normalizeExpression

normalizeProgram :: Parse.Expression -> TopLevel
normalizeProgram x =
  let normalized = fromRight $ Normalize.normalizeStatement x
  in case normalized of
       STopLevel topLevel -> topLevel
       _ -> error "Must be top level!"

exhaustivelyExpandMacros ::
     (MonadError Error m, MonadIO m) => Parse.Expression -> m Parse.Expression
exhaustivelyExpandMacros = exhaustM expansionPass

expansionPass ::
     (MonadError Error m, MonadIO m) => Parse.Expression -> m Parse.Expression
expansionPass x = expandMacros macroDefinitions x
  where
    macroDefinitions = extractMacroDefinitions $ normalizeProgram x

stripMacroDefinitions :: TopLevel -> TopLevel
stripMacroDefinitions = statements %~ filter (not . isMacroDefinition)
  where
    isMacroDefinition (SMacroDefinition _) = True
    isMacroDefinition _ = False

testSource :: String
testSource =
  "(defmacro quasiquote; This is a quasiquote macro\n\
  \ (((list (SExpression xs)))\n\
  \  (let ((quasiquoteElem (fn (x) (case x\n\
  \                                 ((SExpression (list 'unquote x))\n\
  \                                  (SExpression (list 'list x)))\n\
  \                                 ((SExpression (list 'unquote-splicing x))\n\
  \                                  x)\n\
  \                                 (atom\n\
  \                                  (SExpression\n\
  \                                   (list 'list\n\
  \                                    (SExpression (list 'quasiquote atom)))))))))\n\
  \;blargh\n\
  \   (list (SExpression (list 'SExpression (SExpression (list 'concat (SExpression (: 'list (map quasiquoteElem xs))))))))))\n\
  \ (((list atom)) (list (SExpression (list 'quote atom)))))\n\
  \\n\
  \(defmacro when\n\
  \ (((list condition body)) (list `(if' ,condition ,body (error \"WHEN\")))))\n\
  \\n\
  \(= main (IO Unit)\n\
  \ (() (return (when (== 1 1) (putStrLn \"Hi!\")))))"
  -- \ (() (return (``(a ,,(LiteralInt (+ 1 2)))))))"
  -- \ (() (return `(begin `(list ,@((LiteralInt 1) ,@(list (LiteralInt (+ 1 1))) (LiteralInt 4))) end))))"
  -- \ (() (return `(1 ,@(2)))))"
