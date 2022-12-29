module Axel.Parse.Args where
import Axel
import qualified Prelude as AxelRuntime_GHCPrelude
import qualified Axel.Parse.AST as AxelRuntime_AST
import qualified Axel.Sourcemap as AxelRuntime_Sourcemap
import Axel.Prelude
import Control.Applicative((<**>))
import Data.Foldable(asum)
import qualified Data.Text as T
import Options.Applicative(Parser,ParserInfo,argument,command,fullDesc,helper,hsubparser,info,metavar,progDesc,str)
data FileCommand = ConvertFile FilePath|RunFile FilePath|FormatFile FilePath deriving ()
data ProjectCommand = ConvertProject|FormatProject|RunProject deriving ()
data Command = FileCommand FileCommand|ProjectCommand ProjectCommand|Version deriving ()
experimental :: () => ((->) String String)
experimental  = ((<>) "(EXPERIMENTAL) ")
command' :: () => ((->) String ((->) (ParserInfo a) (Parser a)))
command' name parserInfo = (hsubparser ((<>) (command name parserInfo) (metavar name)))
fileCommandParser :: () => (Parser FileCommand)
fileCommandParser  = (asum [convertFileCommand,formatFileCommand,runFileCommand]) where {convertFileCommand  = (command' "convert" (filePathParser ConvertFile (experimental "Convert a Haskell file to Axel")));runFileCommand  = (command' "run" (filePathParser RunFile "Transpile and run an Axel file"));formatFileCommand  = (command' "format" (filePathParser FormatFile "Format an Axel file"));filePathParser ctor desc = (info ((<$>) ((.) ctor ((.) FilePath T.pack)) (argument str (metavar "FILE"))) (progDesc desc))}
projectCommandParser :: () => (Parser ProjectCommand)
projectCommandParser  = (asum [convertProjectCommand,formatProjectCommand,runProjectCommand]) where {convertProjectCommand  = (command' "convert" (info (pure ConvertProject) (progDesc "Convert the Haskell project to Axel")));formatProjectCommand  = (command' "format" (info (pure FormatProject) (progDesc "Format the project")));runProjectCommand  = (command' "run" (info (pure RunProject) (progDesc "Build and run the project")))}
commandParserInfo :: () => (ParserInfo Command)
commandParserInfo  = (let {subparsers = (asum [fileCommand,projectCommand,versionCommand])} in (info ((<**>) subparsers helper) fullDesc)) where {fileCommand  = (command' "file" (info ((<$>) FileCommand fileCommandParser) (progDesc "Run file-specific commands")));projectCommand  = (command' "project" (info ((<$>) ProjectCommand projectCommandParser) (progDesc "Run project-wide commands")));versionCommand  = (command' "version" (info (pure Version) (progDesc "Display the version of the Axel compiler")))}