{-|
Module      : Haste.GAPI.Request.RequestM
Description : Monad for use in Requests
Copyright   : (c) Jonathan Skårstedt, 2016
License     : MIT
Maintainer  : jonathan.skarstedt@gmail.com
Stability   : experimental
Portability : Haste

Contains the Request Monad with all its instances
-}
module Haste.GAPI.Request.RequestM (
  Error,
  RequestM(..),
  liftIO
  ) where

import Haste
import Control.Monad
import Data.Functor
import Haste.Concurrent
import Control.Monad.IO.Class
import Control.Applicative
import qualified Haste.JSString as J

-- | A default error type for RequestM, will be customisable in the future.
type Error = JSString

-- | An eDSL for requests.
newtype RequestM a = Req {unR :: CIO (Either Error a)}

-- | RequestM can perform CIO actions
instance MonadConc RequestM where
  liftConc a = Req $ a >>= return . Right
  fork (Req c) = Req $ (fork $ void c) >>= return . Right

-- | RequestM can perform IO actions
instance MonadIO RequestM where
  liftIO a = Req $ liftIO a >>= return . Right

-- | RequestM is a monad
instance Monad RequestM where
  fail err = Req . return . Left $ J.pack err
  return a = Req . return $ Right a
  (Req a) >>= f = Req $ do
    a' <- a
    case a' of
      Right good -> unR $ f good
      Left  bad  -> do
        return $ Left bad

-- | Applicative is defined so that it builds under GHC 7.8
instance Applicative RequestM where
  (<*>) = ap
  pure = return

-- | Functor is defined so that it builds under GHC 7.8
instance Functor RequestM where
  fmap f m = m >>= return . f
