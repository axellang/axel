module RecordSyntax where
import Axel
import qualified Prelude as GHCPrelude
import qualified Axel.Parse.AST as AST
data Foo = Foo {fooaXEL_SYMBOL_CARET_ :: Int,bar :: String} deriving ()
foo :: () => Foo
foo  = (Foo {fooaXEL_SYMBOL_CARET_ = 1,bar = "test"})
