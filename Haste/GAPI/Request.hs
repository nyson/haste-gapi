{-# LANGUAGE OverloadedStrings #-}
module Haste.GAPI.Request where

import Haste.GAPI.Promise
import Haste.Foreign
import Haste.Concurrent

import Control.Applicative
import Control.Monad
import Data.Default

import qualified Haste.JSString as JS
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

jsExecuteRequestThen :: String -> JSAny -> IO () -> IO JSAny
jsExecuteRequestThen = ffi "function(p, ps) {\
\ return gapi.client.request({'path': p, 'params': ps}).then({

});\
\ \
\}"

withRequest :: Request -> Promise -> IO ()
withRequest r p = do re <- jsCreateRequest (path r) (toAny $ params r)
                     applyPromise re p
  

