{-# LANGUAGE OverloadedStrings #-}

module Haste.GAPI.Request.Types where

{- TODO:
* Better types?
* Rework request structure?
* Hiding rawRequest and accessing it by other functions instead?

-}
import Haste
import Haste.Foreign
import Data.Default
import qualified Haste.JSString as J
-- import Control.Applicative

-- import qualified Haste.JSString as JS

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
                     
instance FromAny Params where
  fromAny a = Params <$> getKV a
    
instance ToAny Params where
  toAny (Params ps) = let objField (k,v) = (k, toAny $ v)
                      in toObject $ map objField ps 

instance Default Params where
  def = Params []


-- | Request with parameters and everything
data Request = Request { path    :: Path,
                         method  :: JSString,
                         params  :: Params,
                         headers :: JSString,
                         body    :: JSString}

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
instance Default Request where
  def = rawRequest "" $ Params []


-- | Creates a request by manually entering request path and parameters
rawRequest :: JSString -> Params -> Request
rawRequest p ps = Request { path = p,
                            method = "GET",
                            params = ps,
                            headers = "",
                            body = "" }

