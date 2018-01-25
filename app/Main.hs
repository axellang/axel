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
import qualified Lihsp.Parse as Parse (Expression(SExpression, Symbol))
import Lihsp.Parse (runSingle)
import Lihsp.Utils.Recursion (Recursive(bottomUpFmap))

main :: IO ()
main = do
  result <-
    expansionPass $
    parse
      "(begin \
      \ (defmacro y\
      \  (((list z)) (return (list z))))\

      \ (defmacro x\
      \  ((_) (return (list (Literal-int 1) (Literal-int 2) (Literal-int (y 4))))))\

      \ (= main (IO Unit)\
      \  (() (return (+ 1 (x 1))))))"
  result2 <- expansionPass result
  putStrLn $ toHaskell $ stripMacroDefinitions $ normalizeProgram result2

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

-- http://www.lispworks.com/documentation/HyperSpec/Body/02_df.htm
quote :: Parse.Expression -> Parse.Expression
quote x = Parse.SExpression [Parse.Symbol "quote", x]

quasiquoteSexp :: Parse.Expression -> [Parse.Expression]
quasiquoteSexp (Parse.SExpression exprs) =
  case exprs of
    [Parse.Symbol "unquote", x] -> [Parse.SExpression [Parse.Symbol "list", x]]
    Parse.Symbol "unquoteSplicing":xs -> xs
    _ ->
      [ Parse.SExpression $
        Parse.Symbol "append" : concatMap quasiquoteSexp exprs
      ]
quasiquoteSexp x = [quote x]

quasiquote :: Parse.Expression -> Parse.Expression
quasiquote = head . quasiquoteSexp

expandQuasiquote :: Parse.Expression -> Parse.Expression
expandQuasiquote =
  bottomUpFmap $ \expr ->
    case expr of
      Parse.SExpression [Parse.Symbol "quasiquoteAtom", x] ->
        case x of
          Parse.SExpression _ -> x
          _ -> quasiquote x
      Parse.SExpression [Parse.Symbol "quasiquoteSexp", x] ->
        case x of
          Parse.SExpression _ -> quasiquote x
          _ -> x
      _ -> expr
