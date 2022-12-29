module TypeClassInstanceSyntax where
import Axel
import qualified Prelude as AxelRuntime_GHCPrelude
import qualified Axel.Parse.AST as AxelRuntime_AST
import qualified Axel.Sourcemap as AxelRuntime_Sourcemap
instance () => (FooaXEL_SYMBOL_CARET_ Int String) where {foo  = ()}
instance (Quux) => (Bar Int String) where {bar  = ()}
instance ((SuperFoo a),(SuperFoo b)) => (Baz Int String) where {baz a b c = ()}