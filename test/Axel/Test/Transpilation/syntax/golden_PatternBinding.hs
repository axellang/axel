module PatternBinding where
import Axel
import qualified Prelude as AxelRuntime_GHCPrelude
import qualified Axel.Parse.AST as AxelRuntime_AST
import qualified Axel.Sourcemap as AxelRuntime_Sourcemap
foo (bar@baz) = bar
bar  = (case quux of {(bar@(Just _)) -> bar})