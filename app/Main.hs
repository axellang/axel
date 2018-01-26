-- TODO Replace all this with an actual application (instead of test programs).
module Main where

import Control.Lens.Operators ((%~))
import Control.Monad.Except (runExceptT)

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
import Lihsp.Parse (runSingle)

main :: IO ()
main = do
  let result = parse testSource
  result1 <- pass result
  result2 <- pass result1
  result3 <- pass result2
  result4 <- pass result3
  result5 <- pass result4
  result6 <- pass result5
  _ <- pass result6
  return ()
  where
    pass lastResult = do
      nextResult <- expansionPass lastResult
      putStrLn $ toHaskell $ normalizeProgram nextResult
      putStrLn ""
      return nextResult

fromRight :: Either Error b -> b
fromRight x =
  case x of
    Right result -> result
    Left (MacroError err) -> error err
    Left err -> error $ show err

parse :: String -> Parse.Expression
parse = fromRight . runSingle

normalizeExpr :: Parse.Expression -> AST.Expression
normalizeExpr = fromRight . Normalize.normalizeExpression

normalizeProgram :: Parse.Expression -> TopLevel
normalizeProgram x =
  let normalized = fromRight $ Normalize.normalizeStatement x
  in case normalized of
       STopLevel topLevel -> topLevel
       _ -> error "Must be top level!"

expansionPass :: Parse.Expression -> IO Parse.Expression
expansionPass x = fromRight <$> runExceptT (expandMacros macroDefinitions x)
  where
    macroDefinitions = extractMacroDefinitions $ normalizeProgram x

stripMacroDefinitions :: TopLevel -> TopLevel
stripMacroDefinitions = statements %~ filter (not . isMacroDefinition)
  where
    isMacroDefinition (SMacroDefinition _) = True
    isMacroDefinition _ = False

-- TODO Why aren't the numbers lifted on parse into `LiteralInt`s?
testSource :: String
testSource =
  "(begin\
      \ (defmacro quasiquote\
      \  (((list (SExpression xs)))\
      \   (let ((quasiquoteElem (fn (x) (case x\
      \                                  ((SExpression (list 'unquote x))\
      \                                   (SExpression (list 'list x)))\
      \                                  ((SExpression (list 'unquote-splicing x))\
      \                                   x)\
      \                                  (atom\
      \                                   (SExpression\
      \                                    (list 'list\
      \                                     (SExpression (list 'quasiquote atom)))))))))\
      \    (list (SExpression (list 'SExpression (SExpression (list 'concat (SExpression (: 'list (map quasiquoteElem xs))))))))))\
      \  (((list atom)) (list (SExpression (list 'quote atom)))))\

      \ (defmacro when\
      \  (((list condition body)) (list `(if' ,condition ,body (error \"WHEN\")))))\

      \ (= main (IO Unit)\
      \  (() (return (``(a ,,(LiteralInt (+ 1 2))))))))"
      -- \  (() (return (when (== 1 1) (putStrLn \"Hi!\"))))))"
      -- \  (() (return `(begin `(list ,@((LiteralInt 1) ,@(list (LiteralInt (+ 1 1))) (LiteralInt 4))) end)))))"
      -- \  (() (return `(1 ,@(2))))))"
