{-# LANGUAGE OverloadedStrings #-}
module Haste.GAPI.Request where

import Haste.GAPI.Promise
import Haste.Foreign
import Haste.Concurrent

import Control.Applicative
import Control.Monad
import Data.Default

import qualified Haste.JSString as JS

-- | Parameters for a GAPI request
data Params = Params [(String, String)]
            deriving Show

instance ToAny Params where
  toAny (Params ps) = let objField (k,v) = (JS.pack k, toAny $ JS.pack v)
                      in toObject $ map objField ps 

-- | Request with parameters and everything
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



-- | Creates a request by manually entering request path and parameters
rawRequest :: String -> [(String, String)] -> Request
rawRequest p kv = Request { path = p,
                            method = "GET",
                            params = Params kv,
                            headers = "",
                            body = "" }


-- | Applies request and executes the given Promise
withRequest :: Request -> Promise -> IO ()
withRequest r p = do re <- jsCreateRequest (path r) (toAny $ params r)
                     applyPromise re p

-- | Creates a request object 
jsCreateRequest :: String -> JSAny -> IO JSAny
jsCreateRequest = ffi "function(p, ps) {\
\return gapi.client.request({'path': p, 'params': ps})\
\}"

-- | Executes a request and performs the given action as a continuation
jsExecuteRequestThen :: String -> JSAny -> (Response -> IO ()) -> IO ()
jsExecuteRequestThen = ffi "function(p, ps, callback) {\
\ console.debug('invoked jsExecuteRequestThen');\ 
\ gapi.client.request({'path': p, 'params': ps}).then({\
\'then': function(resp) {console.debug('wtf'); callback(resp);}, \
\'error': function(err) {console.debug('error'); console.debug(err);}\
\});\
\}"


-- | Executes a request, blocking while waiting for the result and then
-- return the finished equation
cRequest :: Request -> CIO JSAny
cRequest r = do
  v <- newEmptyMVar
  liftIO . withRequest r
    $ Promise (concurrent . putMVar v)
    $ \_ -> putStrLn "Error"
  takeMVar v
  


data RequestM a = RequestM a

instance Functor RequestM where
  fmap f (RequestM a) = RequestM $ f a

instance Applicative RequestM where
  (RequestM f) <*> (RequestM a) = RequestM $ f a
  pure = RequestM

instance Monad RequestM where
  (RequestM a) >>= f = f a
  return = RequestM

instance MonadConc RequestM where
  liftConc (C a) = RequestM a
  fork = Haste.Concurrent.fork


execute (RequestM a) = a

