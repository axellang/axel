module Main where
import Axel
import qualified Prelude as AxelRuntime_GHCPrelude
import qualified Axel.Parse.AST as AxelRuntime_AST
import qualified Axel.Sourcemap as AxelRuntime_Sourcemap
import Axel.Eff.App(AppEffs,runApp)
import Axel.Eff.Console(putStrLn)
import Axel.Eff.Ghci(withGhci)
import Axel.Haskell.Cabal(axelVersion)
import Axel.Haskell.File(convertFileInPlace,formatFileInPlace,transpileFileInPlace)
import Axel.Haskell.Project(buildProject,convertProject,formatProject,runProject)
import Axel.Parse.Args(Command(FileCommand,ProjectCommand,Version),FileCommand(ConvertFile,RunFile,FormatFile),ProjectCommand(ConvertProject,FormatProject,RunProject),commandParserInfo)
import Control.Monad(void)
import qualified Data.Map as Map(empty)
import qualified Effectful as Eff
import qualified Effectful.State.Static.Local as Eff
import Options.Applicative(execParser)
import Prelude hiding (putStrLn)
app :: () => ((->) Command (Eff.Eff AppEffs ()))
app (FileCommand fileCommand) = (case fileCommand of {(ConvertFile filePath) -> (void (convertFileInPlace filePath));(FormatFile filePath) -> (formatFileInPlace filePath);(RunFile filePath) -> (void (Eff.evalState Map.empty (withGhci (transpileFileInPlace filePath))))})
app (ProjectCommand projectCommand) = (case projectCommand of {ConvertProject -> convertProject;FormatProject -> formatProject;RunProject -> ((>>) buildProject runProject)})
app Version = (putStrLn ((<>) "Axel version " axelVersion))
main :: () => (IO ())
main  = ((>>=) (execParser commandParserInfo) (\modeCommand -> (runApp (app modeCommand))))