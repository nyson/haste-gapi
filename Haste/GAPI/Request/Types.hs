{-# LANGUAGE OverloadedStrings #-}

module Haste.GAPI.Request.Types where

{- TODO:
* Better types?
* Rework request structure?
* Hiding rawRequest and accessing it by other functions instead?

-}
import Haste.Foreign
import Data.Default
import qualified Haste.JSString as JS

getKV :: JSAny -> IO [(String, String)]
getKV = ffi "(function(obj) {\
\var out = [];\
\for(i in obj) { out.push([i, obj[i] ]); }\
\return out;\
\})"

-- | Path for requests
type Path = String

-- | Parameters for a GAPI request
data Params = Params [(String, String)]
            deriving Show
                     
instance FromAny Params where
  fromAny a = Params <$> getKV a
    
instance ToAny Params where
  toAny (Params ps) = let objField (k,v) = (JS.pack k, toAny $ JS.pack v)
                      in toObject $ map objField ps 

instance Default Params where
  def = Params []


-- | Request with parameters and everything
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
  def = rawRequest "" $ Params []


-- | Creates a request by manually entering request path and parameters
rawRequest :: String -> Params -> Request
rawRequest p ps = Request { path = p,
                            method = "GET",
                            params = ps,
                            headers = "",
                            body = "" }

