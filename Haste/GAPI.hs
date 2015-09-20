{-# LANGUAGE OverloadedStrings #-}
module Haste.GAPI where 

import Haste.Foreign
import qualified Haste.JSString as J
import Control.Monad
import Control.Applicative

-- Datatypes -----------------------------------------------------------------
-- | Common Google API config
data Config = Config {clientID  :: String,
                      apiKey    :: String,
                      scopes    :: String,
                      immediate :: Bool}

instance ToAny Config where
  toAny cfg = toObject $ map objPack [("clientID",  toAny $ clientID cfg),
                                      ("apiKey",    toAny $ apiKey cfg),
                                      ("scopes",    toAny $ scopes cfg),
                                      ("immediate", toAny $ immediate cfg)]
    where objPack (k, v) = (J.pack k, toAny v)

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

-- Functions -----------------------------------------------------------------
-- | Load function to push to the Google APIs; Combine with Config and an
loadClient :: Config -> IO () -> IO ()
loadClient = ffi "function(cfg, auth){\
\gapi.client.setApiKey(cfg.apiKey); \
\window.setTimeout(auth, 1);\
\console.debug('loadClient')}"

auth :: Config -> (OAuth2Token -> IO ()) -> IO ()
auth = ffi "function(cfg, ah)\
\{gapi.auth.authorize({\
 \'client_id': cfg.clientID, \
 \'scope': cfg.scopes, \
 \'immediate': cfg.immediate}, \
\ah); \
\console.debug('auth')}"

exportLoaderSymbol :: IO () -> IO ()
exportLoaderSymbol = ffi "function(f) {window.GAPILoader = f;}"

loadGAPI :: Config -> (OAuth2Token -> IO ()) -> IO ()
loadGAPI cfg handler
  = exportLoaderSymbol $ loadClient cfg $ auth cfg handler


