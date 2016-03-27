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


type Error = JSString

-- | Type responsible for executing sensible requests.
newtype RequestM a = Req {unR :: CIO (Either Error a)}

instance MonadConc RequestM where
  liftConc a = Req $ a >>= return . Right
  fork (Req c) = Req $ (fork $ void c) >>= return . Right

instance MonadIO RequestM where
  liftIO a = Req $ liftIO a >>= return . Right

instance Monad RequestM where
  fail err = Req . return . Left $ J.pack err
  return a = Req . return $ Right a
  (Req a) >>= f = Req $ do
    a' <- a
    case a' of
      Right good -> unR $ f good
      Left bad -> do
--        liftIO $ putStrLn bad
        return $ Left bad

instance Applicative RequestM where
  (<*>) = ap
  pure = return

instance Functor RequestM where
  fmap f m = m >>= return . f
