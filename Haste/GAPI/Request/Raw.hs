{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Haste.GAPI.Request.Raw
Description : JavaScript functions needed by Request handling. 
Copyright   : (c) Jonathan Skårstedt, 2016
License     : MIT
Maintainer  : jonathan.skarstedt@gmail.com
Stability   : experimental
Portability : Haste

Contains JavaScript foreign functions for use within the request handler.
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

-- | Returns an Anys children as a Key-value array
getKV :: JSAny -> IO [(JSString, JSString)]
getKV = ffi "(function(obj) {\
\var out = [];\
\for(i in obj) { out.push([i, obj[i] ]); }\
\return out;\
\})"

-- | Transforms an list of tuples into an object with left value as key and
--    right as value for every element in the list
toKV :: [(JSString, JSString)] -> IO JSAny 
toKV = ffi "(function(a2) {\
\console.debug(a2);\
\var obj = {};\
\ for(var i in a2){\
\ obj[a2[i][0]] = a2[i][1];\
\ }\
\})"
