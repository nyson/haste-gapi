{-# LANGUAGE OverloadedStrings #-}
import Haste.GAPI

import Haste.DOM
import Haste.Foreign
import qualified Auth

config = Config {
  -- Your Google API Client ID here 
  clientID = "",
  -- The key for your selected API. Not mandatory for all APIs.
  apiKey = "",
  -- scopes, in this case google plus
  scopes = "https://www.googleapis.com/auth/plus.login",
  -- If true, token is refreshed behind the scenes, not showing any UI.
  immediate = False}

-- Putting stuff to a logging element
domPut :: String -> IO ()
domPut str = do
  entry <- newElem "li" `with` ["innerHTML" =: str]
  withElem "log" (`appendChild` entry)

-- Replace Auth.cfg with your own config
main = loadGAPI' "loadgapi" Auth.config $ \token -> do
  if oa2success token
    then domPut "Successful GAPI Login!"
    else domPut $ "GAPI Error" ++ errorMsg token

  -- Getting a new token
  token' <- getToken
  domPut $ "New token: " ++ show token'

  -- Loading the Google Plus API library with version 'v1'
  withLibrary (Lib "plus" "v1") $ domPut "Loaded G+ Library!"
