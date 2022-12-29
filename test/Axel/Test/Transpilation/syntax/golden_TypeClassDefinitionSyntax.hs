module TypeClassDefinitionSyntax where
import Axel
import qualified Prelude as AxelRuntime_GHCPrelude
import qualified Axel.Parse.AST as AxelRuntime_AST
import qualified Axel.Sourcemap as AxelRuntime_Sourcemap
class () => (FooaXEL_SYMBOL_CARET_ a b) where {foo :: () => ((->) a b)}
class (Quux) => (Bar a b) where {bar :: () => ((->) a b)}
class ((SuperFoo a),(SuperBar b)) => (Baz a b) where {baz :: () => ((->) a b)}