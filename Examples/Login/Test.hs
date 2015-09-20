{-# LANGUAGE OverloadedStrings #-}
import Haste.GAPI

import Haste.DOM

cfg = Config {
  clientID = "199037052348-24kbrq4ats2hn4as626hgq5iuonv5i73.apps.googleusercontent.com",
  apiKey = "",
  scopes = "https://www.googleapis.com/auth/plus.login",
  immediate = False}

main = loadGAPI cfg $ \token -> do
  elem <- do
    e <- newElem "p"
    case token of
     OA2Success {} ->
       set e ["textContent" =: "Successful GAPI Login!"]
     OA2Error {errorMsg= msg} ->
       set e ["textContent" =: ("GAPI Error: " ++ msg)]
    return e
  documentBody `appendChild` elem
