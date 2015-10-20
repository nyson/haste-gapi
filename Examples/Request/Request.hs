{-# LANGUAGE OverloadedStrings #-}
import Haste.GAPI

import Haste
import Haste.DOM
import Haste.Foreign
import Haste.Concurrent

import qualified Haste.JSString as J

import qualified Auth
import Haste.GAPI.Request
import Data.Default

class ToHTML a where
  toHtml :: a -> String

instance ToHTML Request where
  toHtml (Request path method params headers body) = "<h3>Request:</h3>" ++ dd 
    where ddfy :: (String, String) -> String
          ddfy (a,b) = let b' = if b == "" then "Empty" else b               
                       in "<dt>" ++ a ++ "</dt>" ++ "<dd>" ++ b' ++ "</dd>"
          
          ddList :: [(String, String)] -> String
          ddList ls = "<dl>" ++ concatMap ddfy ls ++ "</dl>"
          
          dd = ddList [("Path", path), ("Params", show params)]

config = Config {
  clientID = "",
  apiKey = "",
  scopes = "https://www.googleapis.com/auth/plus.login",
  immediate = False}

printAny :: JSAny -> IO ()
printAny = ffi "function(any) {console.debug(any); }"

-- Putting stuff to a logging element
domPut :: String -> IO ()
domPut str = do
  entry <- newElem "li" `with` ["innerHTML" =: str]
  withElem "log" (`appendChild` entry)

htmlPut :: ToHTML a => a -> IO ()
htmlPut = domPut . toHtml

-- Replace Auth.cfg with your own config
main = loadGAPI' "loadgapi" Auth.config $ \token -> do
  if oa2success token
    then application 
    else domPut $ "GAPI Error" ++ errorMsg token


-- | This is an example of a promise - a promise is a basic datatype of
-- Haste-GAPI. Its type is below:
-- @
--   data Promise = Promise (Response -> IO ()) (Reason -> IO ())
-- @
-- There's two arguments to this type, first `Response -> IO ()` which
--  signifies a success! The promise has been fulfilled and as such we call
--  the first callback.
--
-- `Reason -> IO ()` is a sadder story however. There has been an error! The
--  reason why is hidden deep in the Reason type (hint: it's just a badly
--  veiled `JSAny`) which can be accessed through the standard gapiError
--  function, or you may preferrably write one of your own
myPromise = Promise success err
  where err :: Reason -> IO ()
        err reason = do
          (gapiError $ domPut . ("Oh no: " ++) . J.unpack) reason
          putStrLn "Bad Request!"
          printAny reason

        success :: Response -> IO ()
        success response = do
          domPut $ "Success! The request was carried out splendidly! "
            ++ "<br />See details in your debugger<br />"
          putStrLn "Good Request"
          printAny response
                

goodRequest = def {path = "plus/v1/people/me"}
badRequest = def {path = "plus/v1/people/mesa"}


cPut :: String -> CIO ()
cPut = liftIO . domPut

cHPut :: ToHTML a => a -> CIO ()
cHPut = liftIO . htmlPut

-- | You don't have to use Haste.Concurrent to use Haste.GAPI.Request,
--   but requests will overlap if we don't give them some time alone.
application = concurrent $ do
  cPut "Successfully logged in!"
  cPut "Trying out requests..."


  cPut "Created a request (and then wait a while)..."
  cHPut goodRequest
  liftIO $ request goodRequest myPromise
  wait 2500

  cPut "Now we let Jar Jar Binks choose the API path..."
  wait 500
  cPut "Oh no, not Jar Jar!"
  cHPut badRequest

  liftIO $ request badRequest myPromise   
  
  


