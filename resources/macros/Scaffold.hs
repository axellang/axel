module Scaffold where

import Axel.Parse.AST
import qualified MacroDefinitionAndEnvironment as MacroDefinitionAndEnvironment

main :: IO ()
main = do
  result <-
    MacroDefinitionAndEnvironment.%%%MACRO_NAME%%% %%%ARGUMENTS%%%
  putStrLn $ unlines $ map toAxel result
