{-# LANGUAGE OverloadedStrings #-}
module Haste.GAPI.Request (
  Promise,
  RequestM (..),
  Reason,
  Response, 
  Params (..),
  Request (..),
  Result(..),
  Path (..),
--  FromResult,
  gapiError, rawRequest, withRequest,
  cRequest, req, request,
  has, get, parp, liftIO,
  ) where

import Haste.GAPI.Request.Promise
import Haste.GAPI.Request.RequestM
import Haste.GAPI.Request.Types
import Haste.GAPI.Request.Raw
import Haste.GAPI.Result
import qualified Haste.JSString as J

import Haste.Foreign hiding (has, get)
import Haste.Concurrent 

import Control.Monad
import Control.Applicative

-- | Applies request and executes the given Promise.
withRequest :: Request -> Promise -> IO ()
withRequest r p = do re <- jsCreateRequest (path r) (toAny $ params r)
                     applyPromise re p

-- | Executes a request, blocking while waiting for the result and then
-- return the finished equation
cRequest :: Request -> CIO JSAny
cRequest r = do
  v <- newEmptyMVar
  liftIO . withRequest r
    $ Promise (concurrent . putMVar v)
    $ \_ -> putStrLn "Error"
  takeMVar v

-- | Executes a request
req :: RequestM () -> IO ()
req = concurrent . void . unR

-- | Creates a request
request :: Path -> Params -> RequestM (Result a)
request p params = customRequest . rawRequest p $ params

customRequest :: Request -> RequestM (Result a)
customRequest req = do
  v <- newEmptyMVar
  liftConc . fork . liftIO $ do
    resp <- jsCreateRequest (path req) (toAny $ params req)
    applyPromise resp $ Promise (concurrent . putMVar v . Right . Result)
      (\r -> concurrent . putMVar v . Left . J.pack $
             "Request error: " ++  show req)
  Req $ takeMVar v

merge :: Params -> Params -> Params
merge (Params xs) (Params ys) = Params $ xs ++ ys 
