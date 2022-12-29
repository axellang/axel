module DataDefinitionSyntax where
import Axel
import qualified Prelude as AxelRuntime_GHCPrelude
import qualified Axel.Parse.AST as AxelRuntime_AST
import qualified Axel.Sourcemap as AxelRuntime_Sourcemap
data (BaraXEL_SYMBOL_ASTERISK_ a b) = FoaXEL_SYMBOL_HASH_o (a Int)|Bar b String|Baz deriving ()
data Foo = Foo deriving ()
data Foo' = Foo' deriving (Eq)
data (Foo'' a) = Foo''|a deriving ((Eq a))
data (Foo''' a) = Foo'''|a deriving ((Eq a),Show)