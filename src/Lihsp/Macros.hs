{-# LANGUAGE FlexibleContexts #-}

{-
* Macros
 Macros edit the parse tree, which the compiler converts to an AST which is transpiled into Eta.
 1) Top-level macros are run, with the code forms represented as a list of parse expressions (lists, symbols, and literals).
    The output of the macro programs is rendered as a list and inserted into the file.
 2) Repeat step 1 until no more macros are available.
-}
module Lihsp.Macros where

import Control.Monad.Except (MonadError, throwError)

import Data.Semigroup ((<>))

import Lihsp.Error (Error(MacroError))

-- import qualified Lihsp.Parse as Parse (Expression)
import Lihsp.Transpile (transpile)

import Paths_lihsp (getDataFileName)

import System.Directory (removeFile)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.IO (openTempFile)
import System.Process (readProcessWithExitCode)

withTempFile :: (String -> IO a) -> IO a
withTempFile f = do
  (name, _) <- openTempFile "." "macro.hs"
  result <- f name
  removeFile name
  return result

generateMacroProgram :: (MonadError Error m) => String -> IO (m String)
generateMacroProgram macroSource = do
  footer <- getDataFileName "resources/macros/Footer.hs" >>= readFile
  header <- getDataFileName "resources/macros/Header.hs" >>= readFile
  return $ do
    transpiledMacro <- transpile macroSource
    return $ header <> transpiledMacro <> footer

execInterpreter :: (MonadError Error m) => String -> IO (m String)
execInterpreter fileName = do
  (code, stdout, stderr) <- readProcessWithExitCode "runhaskell" [fileName] ""
  case code of
    ExitSuccess -> return $ return stdout
    ExitFailure _ -> return $ throwError $ MacroError stderr

runMacroProgram :: (MonadError Error m) => String -> IO (m String)
runMacroProgram macroProgram =
  withTempFile $ \tempFile -> do
    writeFile macroProgram tempFile
    execInterpreter tempFile
{-
expandMacroOnce :: String -> String -> Parse.Expression
expandMacroOnce macroSource usageSource = undefined

expandMacros :: String -> String
expandMacros source = undefined
-}
