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

-- | Returns an Anys children as a Key-value array
getKV :: JSAny -> IO [(JSString, JSString)]
getKV = ffi "(function(obj) {\
\var out = [];\
\for(i in obj) { out.push([i, obj[i] ]); }\
\return out;\
\})"

-- | Path for requests
type Path = JSString

-- | Parameters for a GAPI request
data Params = Params [(JSString, JSString)]
            deriving Show

-- | Params can be constructed from a JSAny                     
instance FromAny Params where
  fromAny a = Params <$> getKV a

-- | Params can be converted to a JSAny              
instance ToAny Params where
  toAny (Params ps) = let objField (k,v) = (k, toAny $ v)
                      in toObject $ map objField ps 

-- | Empty Parameters as a default instance
instance Default Params where
  def = Params []

-- | Merge two parameters
merge :: Params -> Params -> Params
merge (Params xs) (Params ys) = Params $ xs ++ ys 

-- | Cons a param with a base value
pcons :: (JSString, JSString) -> Params -> Params
pcons x (Params xs) = Params $ x:xs

-- | Create a set of parameters
params :: [(JSString, JSString)] -> Params
params = Params

-- | Request with parameters and everything
data Request = Request { path    :: Path,
                         method  :: JSString,
                         rparams :: Params,
                         headers :: JSString,
                         body    :: JSString}

-- | The requests can be shown as a debug feature
instance Show Request where
  show (Request p m pms _hs _body)
    = let showDict :: (String, String) -> String
          showDict (a,b) = "\n\t" ++ a ++  ": " ++ b
          showParams (Params p')
            = show $ map (\(a,b) -> (J.unpack a, J.unpack b)) p'
      in "Request: " ++  concatMap showDict [
        ("Path", J.unpack p),
        ("Method", J.unpack m),
        ("Params", showParams pms)
        ]

-- | A default request without any path given         
instance Default Request where
  def = rawRequest "" def 


-- | Creates a request by manually entering request path and parameters
rawRequest :: JSString -> Params -> Request
rawRequest p ps = Request { path    = p,
                            method  = "GET",
                            rparams = ps,
                            headers = "",
                            body    = "" }

