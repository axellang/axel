module TypeSignatureSyntax where
import Axel
import qualified Prelude as GHCPrelude
import qualified Axel.Parse.AST as AST
main :: () => (IO ())
foo :: () => Int
bar :: (Quux) => ((->) Int ((->) Char Bool))
