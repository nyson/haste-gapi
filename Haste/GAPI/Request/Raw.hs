{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Haste.GAPI.Request.Raw
Description : JavaScript functions needed by Request handling. 
Copyright   : (c) Jonathan Skårstedt, 2016
License     : MIT
Maintainer  : jonathan.skarstedt@gmail.com
Stability   : experimental
Portability : Haste

Contains JavaScript foreign functions for use with the request handler.
-}

module Haste.GAPI.Request.Raw where

import Haste
import Haste.Foreign
import Haste.GAPI.Internals.Promise

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

