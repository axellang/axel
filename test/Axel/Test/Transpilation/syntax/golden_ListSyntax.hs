module ListSyntax where
import Axel
import qualified Prelude as GHCPrelude
import qualified Axel.Parse.AST as AST
foo :: () => ([] String)
foo [1,2,3] = [4,5,6]
