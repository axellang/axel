{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where
import Axel.Parse.AST as AST
import Prelude hiding (putStrLn)
import Axel.Eff.App(AppEffs,runApp)
import Axel.Eff.Console(putStrLn)
import Axel.Haskell.File(convertFileInPlace,transpileFileInPlace)
import Axel.Haskell.Project(buildProject,runProject)
import Axel.Haskell.Stack(axelStackageVersion)
import Axel.Macros(ModuleInfo)
import Axel.Parse.Args(Command(Convert,File,Project,Version),commandParser)
import Control.Monad(void)
import Control.Monad.Freer.State(evalState)
import qualified Data.Map as Map(empty)
import Options.Applicative((<**>),execParser,helper,info,progDesc)
app (Convert filePath) = (void (convertFileInPlace filePath))
app (File filePath) = (void ((evalState @ModuleInfo Map.empty) (transpileFileInPlace filePath)))
app (Project ) = ((>>) buildProject runProject)
app (Version ) = (putStrLn ((<>) "Axel version " axelStackageVersion))
app :: (((->) Command) (AppEffs ()))
main  = ((>>=) (execParser (info ((<**>) commandParser helper) (progDesc "The command to run."))) (\modeCommand -> (runApp (app modeCommand))))
main :: (IO ())