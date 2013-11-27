\section[Hooks]{Low level API hooks}

\begin{code}
module Hooks ( Hooks
             , emptyHooks
             , lookupHook
             , getHooked
               -- the hooks:
             , hscFrontendHook
             , runQuasiQuoteHook
             ) where

import DynFlags
import HsTypes
import Name
import HscTypes
import TcRnTypes

import Data.Maybe
import Control.Monad (liftM)

emptyHooks :: Hooks
emptyHooks = Hooks Nothing Nothing

data Hooks = Hooks {
    hscFrontendHook   :: Maybe (ModSummary -> Hsc TcGblEnv)
  , runQuasiQuoteHook :: Maybe (HsQuasiQuote Name -> RnM (HsQuasiQuote Name))
  }     

getHooked :: (Monad f, HasDynFlags f) => (Hooks -> Maybe a) -> a -> f a
getHooked hook def = liftM (lookupHook hook def) getDynFlags

lookupHook :: (Hooks -> Maybe a) -> a -> DynFlags -> a
lookupHook hook def = fromMaybe def . hook . hooks
        
\end{code}
