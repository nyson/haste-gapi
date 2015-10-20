{-# LANGUAGE OverloadedStrings #-}
import Haste.GAPI

import Haste.DOM
import Haste.Foreign
import qualified Auth
import Haste.GAPI.Request
import Data.Default

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

gapi = loadGAPI' "loadgapi" Auth.config $ \token -> do
  if oa2success token
    then domPut "Successful GAPI Login!"
    else domPut $ "GAPI Error" ++ errorMsg token
         
  domPut "Trying out requests..."
  
  let req = def { path="plus/v1/people",
                  params = Params [("query", "Jonathan Skårstedt")]
                }

  domPut $ show req
  request req (Promise
               (\resp -> do domPut "Success! See details in your debugger"
                            printAny resp)
               (\err -> do domPut "Error! See details in your debugger"
                           printAny err)
                )
  

-- Replace Auth.cfg with your own config
main = do gapi
          putStrLn $ "Loaded!" ++ show Auth.config

  
