{-# LANGUAGE OverloadedStrings #-}
module Haste.GAPI.Request where

import Haste.GAPI.Promise
import Haste.Foreign
import Haste.Concurrent

import Control.Applicative
import Control.Monad

-- | Monad which is responsible for GAPI request handling
--   modelled after this paper (ICFP '14):
--   <http://community.haskell.org/~simonmar/papers/haxl-icfp14.pdf
--   There is no Fork>
data Batch a = Done a | Waiting (Batch a)

instance Functor Batch where
  -- Requests that are done can be applied directly 
  fmap f (Done x) = Done $ f x
  -- Waiting requests push down the function to lower levels
  fmap f (Waiting c) = Waiting (fmap f c)

instance Monad Batch where
  return = Done
  -- We just apply with done
  Done a    >>= k = k a
  -- We apply the inner value with done
  Waiting c >>= k = Waiting (c >>= k)

instance Applicative Batch where 
  pure = return
  Done g    <*> Done y    = Done (g y)
  Done g    <*> Waiting w = Waiting (g <$> w)
  Waiting c <*> Done y    = Waiting (c <*> Done y)
  Waiting c <*> Waiting d = Waiting (c <*> d)

-- | fish chains two requests. That is just what fishes do.
-- fish :: (a -> CIO b) -> (b -> CIO c) -> a -> CIO c
-- fish fa fb x = do
--   v <- newMVar
--   fork $ fa x >>= putMVar v
--   takeMVar v >>= fb


data Request = Request { path    :: String,
                         method  :: String,
                         params  :: [(String, JSAny)],
                         headers :: String,
                         body    :: String}

-- | Creates a raw JS request
rawRequest :: String -> [(String, JSAny)] -> Request
rawRequest p kv = Request { path = p,
                            method = "GET",
                            params = kv,
                            headers = "",
                            body = "" }

jsCreateRequest :: String -> [(String, JSAny)] -> IO JSAny
jsCreateRequest = ffi "function(p, p) {\
\return gapi.client.request({'path': p, 'params': ps})}"

request :: Request -> Promise -> IO ()
request r p = do
  raw <- jsCreateRequest (path r) (params r)
  applyPromise raw p
  
