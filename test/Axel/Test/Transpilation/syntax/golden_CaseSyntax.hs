module CaseSyntax where
import Axel
import qualified Prelude as GHCPrelude
import qualified Axel.Parse.AST as AST
main  = (case True of {True -> (putStrLn "Yay!");False -> (error "Impossible!")})
