{-# LANGUAGE OverloadedStrings #-}
module Haste.GAPI.Promise where

import Haste.Foreign
type Response = JSAny
type Reason = JSAny 
data Promise = Promise (Response -> IO ()) (Reason -> IO ())

{- Some kind of MVar so represent the result maybe? -}
instance ToAny Promise where
  toAny (Promise thn err) = toObject [("then", toAny thn),
                                      ("error", toAny err)]

applyPromise :: JSAny -> Promise -> IO ()
applyPromise = ffi "function(action, promise) \
\{action.then(promise.then, promise.error)"
