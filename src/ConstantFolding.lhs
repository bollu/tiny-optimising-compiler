\begin{code}
module ConstantFolding where
import qualified OrderedMap as M
import Control.Monad.State.Strict
import Data.Traversable
import Data.Foldable
import Control.Applicative
import qualified Data.List.NonEmpty as NE
import IR
import Data.Text.Prettyprint.Doc as PP
import PrettyUtils
-- Fold all constants, such that what is left is either (val, val)
-- or (val, constant)


constantFold :: IRProgram -> IRProgram
constantFold (program) = 
\end{code}
