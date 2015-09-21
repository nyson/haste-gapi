{-# LANGUAGE OverloadedStrings #-}
module Haste.GAPI (Config(..),
                   OAuth2Token(..),
                   Library(..),
                   loadGAPI,
                   loadGAPI',
                   oa2success,
                   loadLibrary,
                   loadLibrary'
                  ) where 

import Haste.Foreign
import qualified Haste.JSString as J
import Control.Monad
import Control.Applicative

-- Datatypes -----------------------------------------------------------------
-- | Represents a GAPILibrary with a name and a version
data Library = Lib String String

-- | Common Google API config
data Config = Config {clientID  :: String,
                      apiKey    :: String,
                      scopes    :: String,
                      immediate :: Bool}

instance ToAny Config where
  toAny cfg = toObject [("clientID",  toAny $ clientID cfg),
                        ("apiKey",    toAny $ apiKey cfg),
                        ("scopes",    toAny $ scopes cfg),
                        ("immediate", toAny $ immediate cfg)]

-- | OAuth2Token - how did your authentication fare?
data OAuth2Token = OA2Success { accessToken :: String,
                                expiresIn   :: String,
                                state       :: String}
                 | OA2Error { errorMsg :: String,
                              state    :: String}


instance FromAny OAuth2Token where
  fromAny oa2 = do success <- has oa2 "access_token"
                   if success
                     then OA2Success <$> get oa2 "access_token"
                          <*> get oa2 "expires_in"
                          <*> get oa2 "state"
                     else OA2Error <$> get oa2 "error"
                          <*> get oa2 "state"

-- Exported functions --------------------------------------------------------
-- | Returns true if the token represents a successful authentication
oa2success :: OAuth2Token -> Bool
oa2success OA2Success {} = True
oa2success _ = False

-- | Exports and coordinate loading of the Google API.
loadGAPI :: Config -> (OAuth2Token -> IO ()) -> IO ()
loadGAPI cfg handler
  = exportLoaderSymbol "GAPILoader" $ loadClient cfg $ auth cfg handler

-- | Loads the Google API with a custom loader name
loadGAPI' :: String -> Config -> (OAuth2Token -> IO ()) -> IO ()
loadGAPI' symbol cfg handler
  = exportLoaderSymbol symbol $ loadClient cfg $ auth cfg handler


-- | Returns the token from the current GAPI state
getToken :: IO OAuth2Token
getToken = ffi "function() {return gapi.auth.getToken();}"

-- | Loads a library, and then executes the second argument as a callback
loadLibrary' :: Library -> IO () -> IO ()
loadLibrary' (Lib name version) f = loadLib name version $ Just f

-- | Loads a library
loadLibrary :: Library -> IO ()
loadLibrary (Lib name version) = loadLib name version Nothing


-- FFI Functions and other backendy stuff ------------------------------------
-- | FFI for loading a library
loadLib :: String -> String -> Maybe (IO ()) -> IO ()
loadLib = ffi "function(n, v, c) {\
\if (c !== null) { gapi.client.load(n, v, c); }\
\else { gapi.client.load(n, v); }}"

-- | Loads the GAPI Client 
loadClient :: Config -> IO () -> IO ()
loadClient = ffi "function(cfg, auth){\
\gapi.client.setApiKey(cfg.apiKey); \
\window.setTimeout(auth, 1);}"

-- | Authenticates the user. Should be invoked by loadClient
auth :: Config -> (OAuth2Token -> IO ()) -> IO ()
auth = ffi "function(cfg, ah)\
\{gapi.auth.authorize({\
 \'client_id': cfg.clientID, \
 \'scope': cfg.scopes, \
 \'immediate': cfg.immediate}, \
\ah);}"

-- | Export the loader symbol 
exportLoaderSymbol :: String -> IO () -> IO ()
exportLoaderSymbol = ffi "function(s, f) {window[s] = f;}"



