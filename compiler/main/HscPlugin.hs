module HscPlugin
  ( HscPlugin(..)
  ) where

import TcRnTypes
import DynFlags
import MonadUtils

import Data.Monoid
import Control.Monad

data HscPlugin = HscPlugin { runHscPlugin :: forall m. MonadIO m => DynFlags -> TcGblEnv -> m TcGblEnv }

instance Monoid HscPlugin where
  mempty = HscPlugin $ const return
  (HscPlugin f) `mappend` (HscPlugin g) = HscPlugin (\dynFlags -> f dynFlags >=> g dynFlags)

