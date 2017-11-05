{- |
Module      :  Pancake.Common
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  portable

Utility functions.
-}

module Pancake.Common ( putErrLn ) where
import System.IO
import Control.Monad.IO.Class


-- | Prints a line into stderr.
putErrLn :: MonadIO m => String -> m ()
putErrLn s = liftIO $ do
  hPutStrLn stderr s
  hFlush stderr
