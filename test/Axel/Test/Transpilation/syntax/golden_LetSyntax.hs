module LetSyntax where
import Axel
import qualified Prelude as AxelRuntime_GHCPrelude
import qualified Axel.Parse.AST as AxelRuntime_AST
import qualified Axel.Sourcemap as AxelRuntime_Sourcemap
main  = (let {x = 1;y = 2} in (print ((+) x y)))