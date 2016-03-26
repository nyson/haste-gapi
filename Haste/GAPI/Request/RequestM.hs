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
-- Example: This code will execute a request with a path and a set of
--  parameters (params), uses `fields` for fetching the fields "name" and
--  "email" from the first response and creating `params'` with these
--  fields, and uses them as argument to the second call to request.
-- @
--  do response <- request aPath params
--     params' <- fields ["name", "email"] response
--     response' <- request aPath' params'
-- @
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
