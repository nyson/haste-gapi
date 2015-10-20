{-# LANGUAGE OverloadedStrings #-}
module Haste.GAPI.Request where

import Haste.GAPI.Promise
import Haste.Foreign
import Haste.Concurrent

import Control.Applicative
import Control.Monad
import Data.Default

import qualified Haste.JSString as JS

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


data Params = Params [(String, String)]
            deriving Show


instance ToAny Params where
  toAny (Params ps) = let objField (k,v) = (JS.pack k, toAny $ JS.pack v)
                      in toObject $ map objField ps 

data Request = Request { path    :: String,
                         method  :: String,
                         params  :: Params,
                         headers :: String,
                         body    :: String}

instance Show Request where
  show (Request p m pms hs body)
    = let showDict = ((++) "\n\t" . (\(a,b) -> a ++ ": " ++ b))
      in "Request: " ++ concatMap showDict [("Path", p), ("Method", m),
                                            ("Params", show pms)]
instance Default Request where
  def = rawRequest "" []


-- | Creates a raw JS request
rawRequest :: String -> [(String, String)] -> Request
rawRequest p kv = Request { path = p,
                            method = "GET",
                            params = Params kv,
                            headers = "",
                            body = "" }

jsCreateRequest :: String -> JSAny -> IO JSAny
jsCreateRequest = ffi "function(p, ps) {\
\return gapi.client.request({'path': p, 'params': ps})\
\}"

request :: Request -> Promise -> IO ()
request r p = do re <- jsCreateRequest (path r) (toAny $ params r)
                 applyPromise re p
  
