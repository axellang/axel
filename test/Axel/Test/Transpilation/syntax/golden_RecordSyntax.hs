module RecordSyntax where
import Axel
import qualified Prelude as AxelRuntime_GHCPrelude
import qualified Axel.Parse.AST as AxelRuntime_AST
import qualified Axel.Sourcemap as AxelRuntime_Sourcemap
data Foo = Foo {fooaXEL_SYMBOL_CARET_ :: Int,bar :: String} deriving ()
foo :: () => Foo
foo  = (Foo {fooaXEL_SYMBOL_CARET_ = 1,bar = "test"})