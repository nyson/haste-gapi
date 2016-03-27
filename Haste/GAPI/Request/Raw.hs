{-# LANGUAGE OverloadedStrings #-}
{- Raw JavaScript and ffi for Requests
TODO: Do we really need a separate file for FFI?
-}
module Haste.GAPI.Request.Raw where

import Haste
import Haste.Foreign
import Haste.GAPI.Internals.Promise

-- JavaScript exports --------------------------------------------------------
-- | Creates a request object 
jsCreateRequest :: JSString -> JSAny -> IO JSAny
jsCreateRequest = ffi "(function(p, ps) {\
\return gapi.client.request({'path': p, 'params': ps})\
\})"

-- | Executes a request and performs the given action as a continuation
jsExecuteRequestThen :: JSString -> JSAny -> (Response -> IO ()) -> IO ()
jsExecuteRequestThen = ffi "(function(p, ps, callback) {\
\ gapi.client.request({'path': p, 'params': ps}).then({\
\'then': function(resp) {callback(resp);}, \
\'error': function(err) {console.debug(err);}\
\});\
\})"

