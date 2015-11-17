{-# LANGUAGE OverloadedStrings #-}
module Haste.GAPI.Request ( Promise,
                            RequestM,
                            Reason,
                            Response, 
                            Params,
                            Request,
                            Path,
                            gapiError, 
                            rawRequest,
                            withRequest,
                            cRequest
                          ) where

import Haste.GAPI.Request.Promise
import Haste.Foreign
import Haste.Concurrent

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Default

import qualified Haste.JSString as JS

-- JavaScript exports --------------------------------------------------------
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


-- Types ---------------------------------------------------------------------

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
  def = rawRequest "" $ Params []


-- | Creates a request by manually entering request path and parameters
rawRequest :: String -> Params -> Request
rawRequest p ps = Request { path = p,
                            method = "GET",
                            params = ps,
                            headers = "",
                            body = "" }


-- | Applies request and executes the given Promise
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
newtype RequestM a = Req {unR :: CIO (Either String a)}

instance MonadIO RequestM where
  -- liftIO :: IO a -> RequestM a 
  liftIO = Req . unR . liftIO

instance Monad RequestM where
  -- return :: a -> RequestM a
  return a = Req . return $ Right a
  -- (>>=) :: RequestM a -> (a -> RequestM b) -> RequestM b
  (Req a) >>= f = Req $ do
    a' <- a
    case a' of
      Right good -> unR $ f good
      Left bad -> return $ Left bad

instance Applicative RequestM where
  (<*>) = ap
  pure = return

instance Functor RequestM where
  -- fmap :: (a -> b) -> RequestM a -> RequestM b
  fmap f (Req a) = Req $ do
    a' <- a
    return $ case a' of
      Right good -> Right $ f good
      Left err   -> Left err
  
-- | A Request Path
type Path = String

-- | Executes a request
request :: Path -> Params -> RequestM Response
request p params = undefined

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
