module PatternBinding where
import Axel
import qualified Prelude as GHCPrelude
import qualified Axel.Parse.AST as AST
foo (bar@baz) = bar
bar  = (case quux of {(bar@(Just _)) -> bar})
