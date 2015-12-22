{-# LANGUAGE OverloadedStrings #-}

import Haste
import Haste.DOM (appendChild, with, (=:), newElem, elemById, documentBody)
import Haste.Foreign (ffi)
import Haste.GAPI
import Haste.GAPI.Request hiding (get, has)
import Haste.GAPI.Request.Result as R
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
    name <- R.deepGet response "result.displayName"
    liftIO $ put $ "Hello " ++ name ++ "!"
