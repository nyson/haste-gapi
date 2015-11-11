{-# LANGUAGE OverloadedStrings #-}
module Haste.GAPI.Request where

import Haste.GAPI.Promise
import Haste.Foreign
import Haste.Concurrent

import Control.Applicative
import Control.Monad
import Data.Default

import qualified Haste.JSString as JS

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

withRequest :: Request -> Promise -> IO ()
withRequest r p = do re <- jsCreateRequest (path r) (toAny $ params r)
                     applyPromise re p

jsExecuteRequestThen :: String -> JSAny -> (Response -> IO ()) -> IO ()
jsExecuteRequestThen = ffi "function(p, ps, callback) {\
\ console.debug('invoked jsExecuteRequestThen');\
\ gapi.client.request({'path': p, 'params': ps}).then({\
\'then': function(resp) {console.debug('wtf'); callback(resp);}, \
\'error': function(err) {console.debug('error'); console.debug(err);}\
\});\
\}"

cRequest :: Request -> CIO JSAny
cRequest r = do
  liftIO $ putStrLn $ "creating new mvar..."
  v <- newEmptyMVar
  liftIO $ putStrLn $ "new mvar created!"
  
  let ccb r = concurrent $ do
        liftIO $ putStrLn $ "putting val in v.."
        putMVar v r
        liftIO $ putStrLn $ "v now contains value!"

  liftIO $ putStrLn $ "Executing request..."
  liftIO $ withRequest r $ Promise ccb $ \_ -> putStrLn "Error"
  liftIO $ putStrLn $ "Request executed!"
  takeMVar v
  
                               
