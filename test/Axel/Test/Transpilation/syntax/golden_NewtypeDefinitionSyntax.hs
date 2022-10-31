module NewtypeDefinitionSyntax where
import Axel
import qualified Prelude as GHCPrelude
import qualified Axel.Parse.AST as AST
newtype Foo = Foo Bar deriving ()
newtype (Bar a) = Bar (Baz (a Int)) deriving ()
newtype Foo' = Foo' Bar deriving ()
newtype Foo' = Foo' Bar deriving (Eq)
newtype Foo' = Foo' Bar deriving ((Eq a),Show)
