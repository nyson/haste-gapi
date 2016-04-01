{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Haste.GAPI.Internals.Promise
Description : Mapping to Google API promises
Copyright   : (c) Jonathan Skårstedt, 2016
License     : MIT
Maintainer  : jonathan.skarstedt@gmail.com
Stability   : experimental
Portability : Haste

Wraps Promises as implemented in Google API with a haskell representation. 
-}
module Haste.GAPI.Internals.Promise where

import Haste
import Haste.Foreign

-- | An argument to a fulfilled promise
type Response = JSAny
-- | An argument to a rejected promise 
type Reason = JSAny

-- | A promise is either a true promise or a callback
--    (a default error handler will be provided)
data Promise = Promise (Response -> IO ()) (Reason -> IO ())
             | Callback (Response -> IO ())

-- | Transform a promise to an any (for application)
instance ToAny Promise where
  toAny (Promise thn err) = toObject [("then", toAny thn),
                                      ("error", toAny err)]
  toAny (Callback cbk) = toObject [("then", toAny cbk),
                                   ("error", toAny gapiError)]

-- | Apply a promise to a JSAny 
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
