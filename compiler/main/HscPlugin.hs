module HscPlugin
  ( HscPlugin(..)
  ) where

import TcRnTypes
import DynFlags
import MonadUtils
import HsTypes
import Name

import Data.Monoid
import Control.Monad

data HscPlugin = HscPlugin {
    runHscPlugin :: forall m. MonadIO m => DynFlags -> TcGblEnv -> m TcGblEnv
  , runHscQQ     :: forall m. MonadIO m => DynFlags -> HsQuasiQuote Name -> m (HsQuasiQuote Name)
  }

instance Monoid HscPlugin where
  mempty = HscPlugin {
      runHscPlugin = const return
    , runHscQQ     = const return
    }

  a `mappend` b = HscPlugin {
      runHscPlugin = \dynFlags -> runHscPlugin a dynFlags >=> runHscPlugin b dynFlags
    , runHscQQ     = \dynFlags -> runHscQQ     a dynFlags >=> runHscQQ     b dynFlags
    }
