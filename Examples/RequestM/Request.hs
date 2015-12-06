{-# LANGUAGE OverloadedStrings #-}

import Haste
import Haste.DOM
import Haste.Foreign
import Haste.GAPI
import Haste.GAPI.Request
import Haste.GAPI.GPlus

import Control.Monad.IO.Class
import Data.Default

import Auth

debugAny :: JSAny -> IO ()
debugAny = ffi "function(x) {console.debug(x);}"

put :: String -> IO ()
put s = do
  item <- newElem "li" `with` ["innerText" =: s]  
  elem <- elemById "output"

  case elem of
    Just output -> output `appendChild` item
    Nothing     -> do output <- newElem "ul" `with` ["id" =: "output"]
                      output `appendChild` item
                      documentBody `appendChild` output

main = withGAPI Auth.config $ \t -> case t of
  OA2Error {errorMsg = e} -> putStrLn $ "Error lol " ++ e
  OA2Success {}           -> rexec $ do
    response <- request "plus/v1/people/me" def
    person <- fromResult response
    liftIO $ put $ "Hello " ++ (show . name) person ++ "!"

