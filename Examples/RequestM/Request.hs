{-# LANGUAGE OverloadedStrings #-}

{- A RequestM example that greets you if you have a Google+ profile! -}

import Prelude hiding (lookup)
import Haste
import Haste.JSString (append)
import Haste.DOM (appendChild, with, (=:), newElem, elemById, documentBody)
import Haste.GAPI
import Data.Default
import Auth

-- | Puts a string on page
put :: JSString -> IO ()
put s = do
  item <- newElem "li" `with` ["innerHTML" =: s]  
  elem <- elemById "output"
  case elem of
    Just output -> output `appendChild` item
    Nothing     -> do output <- newElem "ul" `with` ["id" =: "output"]
                      output `appendChild` item
                      documentBody `appendChild` output

main = withGAPI Auth.config $ \token -> case token of
  OA2Success {}           -> putStrLn (show token) >> greet
  OA2Error {errorMsg = e} -> putStrLn
                             $ "I can't greet people with invalid access"
                             ++ " tokens :( (" ++ e ++ ")"
greet :: IO ()
greet = req $ do
    response <- request "plus/v1/people/me" def
    Just name <- lookup response "result.displayName"
    liftIO $ put $ "Hello " `append` name `append` "!"
