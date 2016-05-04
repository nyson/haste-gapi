{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Haste.GAPI.Request.Types
Description : Types used in requests. 
Copyright   : (c) Jonathan Skårstedt, 2016
License     : MIT
Maintainer  : jonathan.skarstedt@gmail.com
Stability   : experimental
Portability : Haste

Contains default types and functions to work on them. 
-}


module Haste.GAPI.Request.Types where

-- GHC 7.8 compatibility
import Data.Functor ((<$>))

import Haste
import Haste.Foreign
import Data.Default
import qualified Haste.JSString as J


-- | Google API paths, usually in the form library\/version\/category\/action.
--    For a full list, please see the
--    <https://developers.google.com/apis-explorer/ Google API Explorer>.
type Path = JSString

-- | Representation of a parameter
type Param = (JSString, JSString)

-- | Data structure of a request. Used by @customRequest@ for creating custom
--    requests. If you're just looking for an easy to use request interface,
--    please use @request@ or @request'@ instead.
data Request = Request {
  -- | Path of the request
  path    :: Path,
  -- | Parameters to use. 
  rparams :: [Param],
  -- | HTTP request method to use. (currently not used)
  method  :: JSString,
  -- | Additional request headers (currently not used)
  headers :: JSString,
  -- | HTTP request body (currently not used)
  body    :: JSString}

-- | The requests can be shown as a debug feature
instance Show Request where
  show (Request p pms m _hs _body) = concat [
    "Request: \n",
    concatMap showDict [
        ("Path", p),
        ("Method", m)
        ],
    "\nParameters: {",
    concatMap showDict pms,
    "}"]
    where showDict (a,b) = J.unpack $ J.concat ["\n\t", a, ": ", b]

-- | A default request without any path given         
instance Default Request where
  def = rawRequest "" def 


-- | Creates a request by manually entering request path and parameters
--    note that if several keys exists, only the last key-value pair will
--    be used.
rawRequest :: JSString -> [Param] -> Request
rawRequest p ps = Request { path    = p,
                            method  = "GET",
                            rparams = ps,
                            headers = "",
                            body    = "" }

