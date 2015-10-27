{-# LANGUAGE OverloadedStrings #-}
import Haste.GAPI

import Haste
import Haste.DOM
import Haste.Foreign
import Haste.Concurrent
import qualified Haste.JSString as J
import Haste.GAPI.Request

import Auth
import Data.Default

-- | Your configuration here
config = Config {
  clientID = "", 
  apiKey = "",
  scopes = "https://www.googleapis.com/auth/plus.login",
  immediate = False}

-- | Main function, must be invoked with onexec
-- REMEMBER: Replace Auth.config with your own config
main = loadGAPI Auth.config $ \token -> do
  if oa2success token
    then application 
    else domPut $ "GAPI Error" ++ errorMsg token

-- | You don't have to use Haste.Concurrent to use Haste.GAPI.Request,
--   but requests will overlap if we don't give them some time alone.
application = concurrent $ do
  -- A working request (given a proper token)
  let goodRequest = def {path = "plus/v1/people/me"}
  liftIO . domPut $ "Successfully logged in, trying out requests..."
  liftIO . domPut $ "Using request with path '" ++ path goodRequest
    ++ "' with no parameters..."

  -- Perform request with a promise. See 'myPromise' below for implementation.
  liftIO $ withRequest goodRequest myPromise
  
  {- We wait for a while to make it more probable requests are performed
      somewhat sequentially -}
  wait 2500 
  liftIO . domPut $ ""
    -- A bad request
  let badRequest = def {path = "plus/v1/people/meesa"}
  liftIO . domPut $ "Now we let Jar Jar Binks (OH NO!) choose the API path, "
     ++ "which will cause the request to fail..."
  liftIO . domPut $ "Using request with path '" ++ path badRequest
    ++ "' with no parameters..."

  liftIO $ withRequest badRequest myPromise   
  
  
-- | This is an example of a promise - a promise is a basic datatype of
--   Haste-GAPI. The data type for promises:
-- 
-- > data Promise = Promise (Response -> IO ()) (Reason -> IO ())
--
-- There's two arguments to this type, first `Response -> IO ()` which
--  signifies a success! The promise has been fulfilled and as such we call
--  the first callback.
--
-- `Reason -> IO ()` is a sadder story however. There has been an error! The
--  reason why is hidden deep in the Reason type (hint: it's just a badly
--  veiled `JSAny`) which can be accessed through the standard gapiError
--  function, or you may preferrably write one of your own
myPromise :: Promise
myPromise = Promise success err
  where success :: Response -> IO ()
        success response = do
          domPut $ "Success! The request was carried out splendidly! "
            ++ " - See details in your debugger<br />"
          putStrLn "Good Request"
          printAny response
          
        err :: Reason -> IO ()
        err reason = do
          gapiError (domPut . ("Erroneous request: " ++)
                     . (++ " - See details in your debugger")
                     . J.unpack) reason
          printAny reason
        

-- Some functions to ease debug prints ---------------------------------------

-- Prints a JSAny to console
printAny :: JSAny -> IO ()
printAny = ffi "function(any) {console.debug(any); }"

-- Putting stuff to a logging element
domPut :: String -> IO ()
domPut str = do
  entry <- newElem "li" `with` ["innerHTML" =: str]
  withElem "log" (`appendChild` entry)

