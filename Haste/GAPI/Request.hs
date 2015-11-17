{-# LANGUAGE OverloadedStrings #-}
module Haste.GAPI.Request where

import Haste.GAPI.Promise
import Haste.Foreign
import Haste.Concurrent

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
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
  

-- | Type responsible for executing sensible requests.
-- Example: This code will execute a request with a path and a set of
--  parameters (params), uses `fields` for fetching the fields "name" and
--  "email" from the first response and creating `params'` with these
--  fields, and uses them as argument to the second call to request.
-- @
--  do response <- request aPath params
--     params' <- fields ["name", "email"] response
--     request aPath' params'
-- @
-- 
data RequestM a = RequestM a

instance Functor RequestM where
  fmap f (RequestM a) = RequestM $ f a

instance Applicative RequestM where
  (RequestM f) <*> (RequestM a) = RequestM $ f a
  pure = RequestM

instance Monad RequestM where
  (RequestM a) >>= f = f a
  return = RequestM

instance MonadIO RequestM where
  liftIO f = RequestM $ undefined
  
-- | A Request Path
type Path = String

-- | Executes a request and returns the result
execute :: RequestM a -> Response
execute (RequestM a) = undefined

-- | Executes a request
request :: Path -> Params -> RequestM Response
request = undefined 

-- | Fetches a value from a response. The return type must have
--   a ToAny instance
fetch :: FromAny a => Response -> String -> RequestM a
fetch = undefined

-- | Puts a value in an object. Must have a ToAny instance
put :: ToAny a => Response -> String -> a -> RequestM ()
put = undefined

-- | Takes fields with the given identifiers from response and stores
--   it in a new Params under the same name.
fields :: [String] -> Response -> RequestM Params
fields = undefined

-- | Takes fields with the first element of each tuple and puts them
--   in a new Params with the second element of that tuple as identifier
fields' :: [(String, String)] -> Response -> RequestM Params
fields' = undefined

merge :: Params -> Params -> Params
merge (Params xs) (Params ys) = Params $ xs ++ ys 
