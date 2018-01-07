{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lihsp.Macros where

import Control.Lens.Operators ((.~))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Function ((&))
import Data.Semigroup ((<>))
import Data.Text (pack, replace, unpack)

import Lihsp.AST (Expression, MacroDefinition, name, traverseExpression)
import Lihsp.Error (Error(MacroError))
import Lihsp.Eval (evalSource)
import qualified Lihsp.Parse as Parse (Expression)
import Lihsp.Parse (parseProgram)
import Lihsp.Utils.Resources (readDataFile)

generateMacroProgram ::
     (MonadIO m) => MacroDefinition -> [Expression] -> m String
generateMacroProgram macroDefinition applicationArguments = do
  fileHeader <- liftIO $ readDataFile "resources/macros/Header.hs"
  fileFooter <- liftIO getFileFooter
  return $ fileHeader <> show fileContents <> fileFooter
  where
    getFileFooter =
      let insertApplicationArguments =
            let applicationArgumentsPlaceholder = "%%%ARGUMENTS%%%"
            in unpack .
               replace
                 applicationArgumentsPlaceholder
                 (pack $ show applicationArguments) .
               pack
          insertDefinitionBody =
            let definitionBodyPlaceholder = "%%%MACRO_DEFINITION%%%"
            in unpack .
               replace definitionBodyPlaceholder (pack $ show macroDefinition) .
               pack
      in insertApplicationArguments . insertDefinitionBody <$>
         readDataFile "resources/macros/Footer.hs"

expandMacroCall ::
     (MonadError Error m, MonadIO m)
  => MacroDefinition
  -> [Expression]
  -> m [Parse.Expression]
expandMacroCall macroDefinition args =
  generateMacroProgram macroDefinition args >>= evalSource >>= parseProgram

expandMacros :: Expression -> IO Expression
expandMacros =
  traverseExpression $ \expression ->
    case macroApplication expression of
      Just (macroName, arguments) -> expandMacroCall
      Nothing -> return expression
