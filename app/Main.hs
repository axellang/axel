{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where
import qualified Axel.Parse.AST as AST
import Prelude hiding (putStrLn)
import Axel.Eff.App(AppEffs,runApp)
import Axel.Eff.Console(putStrLn)
import Axel.Haskell.File(convertFileInPlace,transpileFileInPlace)
import Axel.Haskell.Project(buildProject,runProject)
import Axel.Haskell.Stack(axelStackageVersion)
import Axel.Parse.Args(Command(Convert,File,Project,Version),commandParser)
import Control.Monad(void)
import qualified Data.Map as Map(empty)
import Options.Applicative((<**>),execParser,helper,info,progDesc)
import qualified Polysemy as Sem
import qualified Polysemy.State as Sem
app (Convert filePath) = (void (convertFileInPlace filePath))
app (File filePath) = (void ((Sem.evalState Map.empty) (transpileFileInPlace filePath)))
app (Project ) = ((>>) buildProject runProject)
app (Version ) = (putStrLn ((<>) "Axel version " axelStackageVersion))
app :: (((->) Command) (Sem.Sem AppEffs ()))
main  = ((>>=) (execParser (info ((<**>) commandParser helper) (progDesc "The command to run."))) (\modeCommand -> (runApp (app modeCommand))))
main :: (IO ())