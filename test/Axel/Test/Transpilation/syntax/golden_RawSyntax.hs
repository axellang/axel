module RawSyntax where
import Axel
import qualified Prelude as GHCPrelude
import qualified Axel.Parse.AST as AST
makeLenses ''Foo
main  = putStrLn "Hello, world!"
