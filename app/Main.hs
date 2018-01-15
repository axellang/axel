-- TODO Replace all this with an actual application (instead of test programs).
module Main where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO)

import qualified Lihsp.AST as AST (Expression)
import Lihsp.AST (Statement(SMacroDefinition), ToHaskell(toHaskell))
import Lihsp.Error (Error)
import Lihsp.Macros (expandMacros)
import Lihsp.Normalize (normalizeExpression, normalizeStatement)
import qualified Lihsp.Parse as Parse (Expression)
import Lihsp.Parse (parseProgram)

-- TODO This example fails because the macro definitions aren't expanded themselves.
--      Extracting all `defmacro`s is probably now the next step in the process.
--      Somehow, we have to be able to tell if a macro definition depends on other
--      macros or not.
--
--      Idea: When extracting macro definitions, maybe only those at the top-level
--            of the dependency graph should be counted as actual definitions, and
--            then the expansion algorithm can proceed as normal (and will still
--            produce the correct result without modification)?
--
--      What I really need at this point is to setup a comprehensive test suite.
main :: IO ()
main = quoteProgram >>= putStrLn . toHaskell . fromRight

fromRight :: Either Error b -> b
fromRight = either (error . show) id

parse :: String -> Parse.Expression
parse = head . fromRight . parseProgram

macroProgram :: (MonadIO m) => m (Either Error Parse.Expression)
macroProgram =
  runExceptT $
  expandMacros [macroDefinition1, macroDefinition2] $ parse "(+ 1 (x))"
  where
    macroDefinition1 =
      case fromRight . normalizeStatement $
           parse
             "(defmacro x ((_) (return (SExpression (: (Literal-int 1) (: (Literal-int 2) (y)))))))" of
        SMacroDefinition x -> x
        _ -> error "Invalid macro definition: 1!"
    macroDefinition2 =
      case fromRight . normalizeStatement $
           parse
             "(defmacro y ((_) (return (SExpression (: (Literal-int 3) (mempty))))))" of
        SMacroDefinition x -> x
        _ -> error "Invalid macro definition: 2!"

quoteProgram :: (MonadIO m) => m (Either Error AST.Expression)
quoteProgram =
  runExceptT $ normalizeExpression $ parse "(quote (quote ((quote 1 2 3) 2 3)))"
