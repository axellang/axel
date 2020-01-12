module Main where

import Axel
import Axel.Eff.App (AppEffs, runApp)
import Axel.Eff.Console (putStrLn)
import Axel.Eff.Ghci (withGhci)
import Axel.Haskell.Cabal (axelVersion)
import Axel.Haskell.File
  ( convertFileInPlace
  , formatFileInPlace
  , transpileFileInPlace
  )
import Axel.Haskell.Project
  ( buildProject
  , convertProject
  , formatProject
  , runProject
  )
import Axel.Macros (Backend, HaskellBackendEffs, haskellBackend)
import qualified Axel.Parse.AST as AST
import Axel.Parse.Args
  ( Command(FileCommand, ProjectCommand, Version)
  , FileCommand(ConvertFile, FormatFile, RunFile)
  , ProjectCommand(ConvertProject, FormatProject, RunProject)
  , commandParserInfo
  )
import Control.Monad (void)
import qualified Data.Map as Map (empty)
import Options.Applicative (execParser)
import qualified Polysemy as Sem
import qualified Polysemy.State as Sem
import qualified Prelude as GHCPrelude
import Prelude hiding (putStrLn)

backend :: Backend HaskellBackendEffs
backend = haskellBackend

app :: () => (((->) Command) (Sem.Sem AppEffs ()))
app (FileCommand fileCommand) =
  (case fileCommand of
     (ConvertFile filePath) -> (void (convertFileInPlace filePath))
     (FormatFile filePath) -> (formatFileInPlace filePath)
     (RunFile filePath) ->
       (void
          (Sem.evalState
             Map.empty
             (withGhci (transpileFileInPlace backend filePath)))))
app (ProjectCommand projectCommand) =
  (case projectCommand of
     (ConvertProject) -> convertProject
     (FormatProject) -> formatProject
     (RunProject) -> ((>>) (buildProject backend) runProject))
app (Version) = (putStrLn ((<>) "Axel version " axelVersion))

main :: () => (IO ())
main =
  ((>>=)
     (execParser commandParserInfo)
     (\modeCommand -> (runApp (app modeCommand))))
