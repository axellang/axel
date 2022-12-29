module Axel.Haskell.Macros where
import Axel
import qualified Prelude as AxelRuntime_GHCPrelude
import qualified Axel.Parse.AST as AxelRuntime_AST
import qualified Axel.Sourcemap as AxelRuntime_Sourcemap
import Axel.Prelude
import Axel.Haskell.Language(isOperator)
import qualified Data.Text as T
hygenisizeMacroName :: () => ((->) Text Text)
hygenisizeMacroName oldName = (let {suffix = (aXEL_SYMBOL_IF_ (isOperator (T.unpack oldName)) "%%%%%%%%%%" "_AXEL_AUTOGENERATED_MACRO_DEFINITION")} in (aXEL_SYMBOL_IF_ (T.isSuffixOf suffix oldName) oldName ((<>) oldName suffix)))