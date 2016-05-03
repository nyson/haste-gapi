{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Haste.GAPI.Request
Description : Contains everything Request-related 
Copyright   : (c) Jonathan Skårstedt, 2016
License     : MIT
Maintainer  : jonathan.skarstedt@gmail.com
Stability   : experimental
Portability : Haste

Contains RequestM and run functions.

-}
module Haste.GAPI.Request (
  -- | Running the Request eDSL
  RequestM,
  runR, runRConc,
  -- | == Performing requests
  Path (..),
  request, request',
  -- Will be introduced when custom requests are finished
  -- -- | == Creating requests with custom parameters
  -- Request (..),
  -- customRequest,
  -- | == Using results 
  Result(..),
  has, get, hasAll, val, valOf, children,
  lookupResult, lookupVal,
  -- | Perform an IO Action inside RequestM 
  liftIO,
  -- | Perform a CIO Action inside RequestM 
  liftConc, 
  -- | === Constructing parameters
  Params(),
  parp, merge, pcons, params
  ) where

import Haste.GAPI.Token
import Haste.GAPI.Internals.Promise
import Haste.GAPI.Request.RequestM
import Haste.GAPI.Request.Types (
  Path,
  Params(..), Request(..),
  merge, pcons, params,
  rawRequest)
import Haste.GAPI.Request.Raw
import Haste.GAPI.Result
import qualified Haste.JSString as J

import Haste.Foreign hiding (has, get, hasAll)
import Haste.Concurrent 

import Data.Default 
import Control.Monad
import Control.Applicative

-- | Validates a token 
validateToken :: OAuth2Token -> RequestM ()
validateToken t = do
  valid <- liftIO $ oa2Success t
  unless valid $ fail "The given token is not valid!"
  
-- | Runs the request eDSL. Locking. 
runR :: OAuth2Token -> RequestM () -> IO ()
runR t r = concurrent . void . unR $ validateToken t >> r 

-- | Runs the request eDSL from a concurrent context. Locking.
runRConc :: OAuth2Token -> RequestM () -> CIO ()
runRConc t r = void . unR $ validateToken t >> r  

-- | Creates a request from an API path and a series of parameters.
request :: Path -> Params -> RequestM (Result a)
request p ps = customRequest (rawRequest p ps)

-- | Performs a request from an API path but without any parameters.
request' :: Path -> RequestM (Result a)
request' p = customRequest (rawRequest p def)

-- | Creates a request using a custom request
customRequest :: Request -> RequestM (Result a)
customRequest req = do
  v <- newEmptyMVar
  liftConc . fork . liftIO $ do
    resp <- jsCreateRequest (path req) (toAny $ rparams req)
    applyPromise resp $ Promise (concurrent . putMVar v . Right . toResult)
      (\_r -> concurrent . putMVar v . Left . J.pack $ 
             "Request error: " ++ show req)
  Req $ takeMVar v

