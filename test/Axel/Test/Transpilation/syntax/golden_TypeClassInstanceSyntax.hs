module TypeClassInstanceSyntax where
import Axel
import qualified Prelude as GHCPrelude
import qualified Axel.Parse.AST as AST
instance () => (FooaXEL_SYMBOL_CARET_ Int String) where {foo  = ()}
instance (Quux) => (Bar Int String) where {bar  = ()}
instance ((SuperFoo a),(SuperFoo b)) => (Baz Int String) where {baz a b c = ()}
