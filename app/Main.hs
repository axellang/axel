module Main where

import Control.Monad.Except (runExceptT)

import Lihsp.AST (Statement(SMacroDefinition))
import Lihsp.Macros (expandMacros)
import Lihsp.Normalize (normalizeStatement)
import Lihsp.Parse (parseProgram)

main :: IO ()
main = program >>= print . fromRight
  where
    fromRight = either (error . show) id
    parse = head . fromRight . parseProgram
    macroDefinition =
      case fromRight . normalizeStatement $
           parse
             "(defmacro x ((_) (return (return (SExpression (: (Literal-int 1) (: (Literal-int 2) (mempty))))))))" of
        SMacroDefinition x -> x
        _ -> error "Invalid macro definition!"
    program = runExceptT $ expandMacros [macroDefinition] $ parse "(x)"
