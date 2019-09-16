module Main where
import qualified Prelude as GHCPrelude
import qualified Axel.Parse.AST as AST
import qualified Prelude as GHCPrelude
import qualified Axel.Parse.AST as AST
import Axel.Eff.App(AppEffs,runApp)
import Axel.Eff.Console(putStrLn)
import Axel.Eff.Ghci(withGhci)
import Axel.Haskell.File(convertFileInPlace,formatFileInPlace,transpileFileInPlace)
import Axel.Haskell.Project(buildProject,runProject)
import Axel.Haskell.Stack(axelStackageVersion)
import qualified Axel.Parse.AST as AST
import Axel.Parse.Args(Command(FileCommand,ProjectCommand,Version),FileCommand(ConvertFile,RunFile,FormatFile),ProjectCommand(RunProject),commandParserInfo)
import Control.Monad(void)
import qualified Data.Map as Map(empty)
import Options.Applicative(execParser)
import qualified Polysemy as Sem
import qualified Polysemy.State as Sem
import Prelude hiding (putStrLn)
app (FileCommand fileCommand) = (case fileCommand of {(ConvertFile filePath) -> (void (convertFileInPlace filePath));(FormatFile filePath) -> (formatFileInPlace filePath);(RunFile filePath) -> (void (Sem.evalState Map.empty (withGhci (transpileFileInPlace filePath))))})
app (ProjectCommand projectCommand) = (case projectCommand of {RunProject -> ((>>) buildProject runProject)})
app (Version ) = (putStrLn ((<>) "Axel version " axelStackageVersion))
app :: (((->) Command) (Sem.Sem AppEffs ()))
main  = ((>>=) (execParser commandParserInfo) (\modeCommand -> (runApp (app modeCommand))))
main :: (IO ())