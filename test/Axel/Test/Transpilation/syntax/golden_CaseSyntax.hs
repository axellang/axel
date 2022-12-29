module CaseSyntax where
import Axel
import qualified Prelude as AxelRuntime_GHCPrelude
import qualified Axel.Parse.AST as AxelRuntime_AST
import qualified Axel.Sourcemap as AxelRuntime_Sourcemap
main  = (case True of {True -> (putStrLn "Yay!");False -> (error "Impossible!")})