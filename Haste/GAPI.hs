-- TODO: Comment and stuff
--       Move any library functions to separate module

{-# LANGUAGE OverloadedStrings #-}
module Haste.GAPI (
  -- | == Connecting to the Google API
  withGAPI,
  oa2success,
  getToken,
  Config(..),
  OAuth2Token(..),
  -- | == Creating Requests
  module Haste.GAPI.Request,
  -- | == Handling Results 
  module Haste.GAPI.Result,
  lookupVal
                   ) where 

import Haste.Foreign hiding (get, has)
import qualified Haste.Foreign as FFI
import qualified Haste.JSString as J

import Haste.GAPI.Request 
import Haste.GAPI.Result
import Data.Default
import Control.Monad
import Control.Applicative

-- Datatypes -----------------------------------------------------------------

-- | Google API config
data Config = Config {
  -- | GAPI Client ID to generate an access token from
  clientID  :: String,
  -- | The API key for your application
  apiKey    :: String,
  -- | The availiable scopes for your application
  scopes    :: String,
  -- | If true, the token is refreshed behind the scenes
  immediate :: Bool    
  }
              
instance Show Config where
  show (Config cid key scopes imm)
    = "\nConfig: " ++ concatMap (++ "\n\t") [cid, key, scopes, show imm]

instance ToAny Config where
  toAny cfg = toObject [("clientID",  toAny $ clientID cfg),
                        ("apiKey",    toAny $ apiKey cfg),
                        ("scopes",    toAny $ scopes cfg),
                        ("immediate", toAny $ immediate cfg)]

-- | OAuth2Token 
data OAuth2Token = OA2Success {
  -- | Authenticated access token 
  accessToken :: String,
  -- | Expiration of the token 
  expiresIn   :: String,
  -- | 
  state       :: String
  }
                 | OA2Error {
                     errorMsg :: String,
                     state    :: String}

instance Show OAuth2Token where
  show t = if oa2success t
           then "Success Token: '" ++ shorten (accessToken t)
                ++ "'\n\t for scopes: '" ++ state t
                ++ "'\n\t expires in: "++ expiresIn t ++ "s"
           else "Failure Token: " ++ errorMsg t
    where shorten :: String -> String
          shorten str | length str < 16 = str
                      | otherwise       = take 32 str ++ "..."


instance FromAny OAuth2Token where
  fromAny oa2 = do
    success <- FFI.has oa2 "access_token"
    if success
      then OA2Success <$> FFI.get oa2 "access_token"
           <*> FFI.get oa2 "expires_in"
           <*> FFI.get oa2 "state"
      else OA2Error <$> FFI.get oa2 "error"
           <*> FFI.get oa2 "state"
 
-- Exported functions --------------------------------------------------------
-- | Returns true if the token represents a successful authentication
oa2success :: OAuth2Token -> Bool
oa2success OA2Success {} = True
oa2success _ = False

-- | Loads GAPI and insert relevant headers in the DOM
withGAPI :: Config -> (OAuth2Token -> IO ()) -> IO ()
withGAPI cfg handler = do
  loadGAPI' "GAPILoader" cfg handler
  loadGAPIExternals "GAPILoader"

-- | Exports and coordinate loading of the Google API.
loadGAPI :: Config -> (OAuth2Token -> IO ()) -> IO ()
loadGAPI = loadGAPI' "GAPILoader"

-- | Loads the Google API with a custom loader name
loadGAPI' :: String -> Config -> (OAuth2Token -> IO ()) -> IO ()
loadGAPI' symbol cfg handler
  = exportLoaderSymbol symbol $ loadClient cfg $ auth cfg handler

-- | Returns the token from the current GAPI state
getToken :: IO OAuth2Token
getToken = ffi "(function() {return gapi.auth.getToken();})"
  
-- FFI Functions and other backendy stuff ------------------------------------
-- | Loads a library and then issues a callback
loadLibCallback :: String -> String -> IO () -> IO ()
loadLibCallback = ffi "(function(libraryName, version, callback) { \
\gapi.client.load(libraryName, version, callback); })"

-- -- | Loads a library and then applies the promise 
-- loadLibPromise :: String -> String -> Promise -> IO ()
-- loadLibPromise = ffi "(function(n, v, p) {\
-- \gapi.client.load(n, v).then(p.then, p.error);})"

-- | Loads the GAPI Client 
loadClient :: Config -> IO () -> IO ()
loadClient = ffi "(function(cfg, auth){\
\gapi.client.setApiKey(cfg.apiKey); \
\window.setTimeout(auth, 1);})"

-- | Authenticates the user. Should be invoked by loadClient
auth :: Config -> (OAuth2Token -> IO ()) -> IO ()
auth = ffi "(function(cfg, ah)\
\{gapi.auth.authorize({\
 \'client_id': cfg.clientID, \
 \'scope': cfg.scopes, \
 \'immediate': cfg.immediate}, \
\ah);})"

-- | Export the loader symbol 
exportLoaderSymbol :: String -> IO () -> IO ()
exportLoaderSymbol = ffi "(function(s, f) {window[s] = f;})"

-- | Loads the external GAPI scripts
loadGAPIExternals :: String -> IO ()
loadGAPIExternals = ffi "(function(sym) {\
\var s = document.createElement('script');\
\s.setAttribute('src', 'https://apis.google.com/js/client.js?onload=' + sym);\
\s.setAttribute('type', 'text/javascript');\
\document.head.appendChild(s);})"
