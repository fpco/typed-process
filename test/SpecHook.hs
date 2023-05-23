module SpecHook where

import           Test.Hspec
import           GHC.Conc

hook :: Spec -> Spec
hook spec = runIO (getNumProcessors >>= setNumCapabilities) >> parallel spec
