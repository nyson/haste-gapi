{-# LANGUAGE OverloadedStrings #-}
module Haste.GAPI.Request (
  RequestM (..),
  Params (..),
  Request (..),
  Result(..),
  Path (..),
  -- gapiError, rawRequest, withRequest,
  req, request,
  has, get, parp, liftIO,
  ) where

import Haste.GAPI.Internals.Promise
import Haste.GAPI.Request.RequestM
import Haste.GAPI.Request.Types (
  Path,
  Params(..), Request(..),
  merge, rawRequest)
import Haste.GAPI.Request.Raw
import Haste.GAPI.Result
import qualified Haste.JSString as J

import Haste.Foreign hiding (has, get)
import Haste.Concurrent 

import Control.Monad
import Control.Applicative

-- | Executes a request
req :: RequestM () -> IO ()
req = concurrent . void . unR

-- | Creates a request
request :: Path -> Params -> RequestM (Result a)
request p params = customRequest . rawRequest p $ params

-- | Creates a request with a custom request
customRequest :: Request -> RequestM (Result a)
customRequest req = do
  v <- newEmptyMVar
  liftConc . fork . liftIO $ do
    resp <- jsCreateRequest (path req) (toAny $ params req)
    applyPromise resp $ Promise (concurrent . putMVar v . Right . Result)
      (\r -> concurrent . putMVar v . Left . J.pack $
             "Request error: " ++  show req)
  Req $ takeMVar v

