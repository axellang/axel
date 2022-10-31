module OperatorSectionSyntax where
import Axel
import qualified Prelude as GHCPrelude
import qualified Axel.Parse.AST as AST
foo  = (flip (+) 1)
quux  = ((+) 1 2)
