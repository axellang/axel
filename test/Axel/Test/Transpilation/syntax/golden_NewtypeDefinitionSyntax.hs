module NewtypeDefinitionSyntax where
import Axel
import qualified Prelude as AxelRuntime_GHCPrelude
import qualified Axel.Parse.AST as AxelRuntime_AST
import qualified Axel.Sourcemap as AxelRuntime_Sourcemap
newtype Foo = Foo Bar deriving ()
newtype (Bar a) = Bar (Baz (a Int)) deriving ()
newtype Foo' = Foo' Bar deriving ()
newtype Foo' = Foo' Bar deriving (Eq)
newtype Foo' = Foo' Bar deriving ((Eq a),Show)