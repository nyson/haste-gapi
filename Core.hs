{-# LANGUAGE OverloadedStrings #-}
import Haste.Foreign
import qualified Haste.JSString as J
import Control.Monad
import Control.Applicative

class Promise a where
  then_ :: a -> IO ()

-- | Common Google API config
data Config = Config {clientID :: String,
                      apiKey   :: String,
                      scopes   :: String}

instance ToAny Config where
  toAny cfg = toObject $ map objPack [("clientID", clientID cfg),
                                      ("apiKey", apiKey cfg),
                                      ("scopes", scopes cfg)]
    where objPack (k, v) = (J.pack k, toAny v)

-- | OAuth2Token - how did your authentication fare?
data OAuth2Token = OA2Success {access_token :: String,
                               expires_in  :: String,
                               state       :: String}
                 | OA2Error {error :: String,
                             state :: String}

instance FromAny OAuth2Token where
  fromAny oa2 = do success <- has oa2 "access_token"
                   if success
                     then OA2Success <$> get oa2 "access_token"
                          <*> get oa2 "expires_in"
                          <*> get oa2 "state"
                     else OA2Error <$> get oa2 "error"
                          <*> get oa2 "state"

-- | Load function to push to the Google APIs; Combine with Config and an
loadClient :: Config -> IO () -> IO ()
loadClient = ffi "function(cfg, auth){\
\gapi.client.setApiKey(cfg.apiKey); \
\window.setTimeout(auth, 1);}"

auth :: Config -> (OAuth2Token -> IO ()) -> Bool -> IO ()
auth = ffi "function(cfg, authHandler, immediate)\
\{gapi.auth.authorize({\
 \'client_id': cfg.clientID, \
 \'scope': cfg.scopes, \
 \'immediate': immediate)}, \
\authHandler); \
\}"

loadGapi :: Config -> (OAuth2Token -> IO ()) -> Bool -> IO ()
loadGapi cfg handler immediate
  = export "GAPIloader" (loadClient cfg (auth cfg handler immediate))
