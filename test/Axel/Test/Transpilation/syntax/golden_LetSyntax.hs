module LetSyntax where
import Axel
import qualified Prelude as GHCPrelude
import qualified Axel.Parse.AST as AST
main  = (let {x = 1;y = 2} in (print ((+) x y)))
