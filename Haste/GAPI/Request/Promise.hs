{-# LANGUAGE OverloadedStrings #-}
{- TODO: Fix
* gapiError should probably not be defined here
* should Haste.Promise even be a separate thing?
-}

module Haste.GAPI.Request.Promise where

import qualified Haste.JSString as J

import Haste.Foreign
import Haste.GAPI.Internals
import Haste
import Control.Applicative
import Control.Monad
import Data.Maybe

type Response = JSAny
type Reason = JSAny 
data Promise = Promise (Response -> IO ()) (Reason -> IO ())
             | Callback (Response -> IO ())

instance ToAny Promise where
  toAny (Promise thn err) = toObject [("then", toAny thn),
                                      ("error", toAny err)]
  toAny (Callback cbk) = toObject [("then", toAny cbk),
                                   ("error", toAny gapiError)]

applyPromise :: JSAny -> Promise -> IO ()
applyPromise = ffi "(function(action, p) {action.then(p.then, p.error);})"

-- | Default error handler in promises
gapiError :: (JSString -> IO ()) -> Reason -> IO ()
gapiError action reason
  = do msg <- lookupAny reason "result.error.message"
       case msg of
        Just m -> do str <- fromAny m
                     action str
        Nothing -> putStrLn "Malformed response!"
