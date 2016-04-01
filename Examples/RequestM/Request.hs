{-# LANGUAGE OverloadedStrings #-}

{- A RequestM example that greets you if you have a Google+ profile! -}

import Prelude hiding (lookup)
import Haste
import Haste.JSString (pack)
import Haste.DOM.JSString (appendChild, with, (=:), newElem, elemById, documentBody)
import Haste.GAPI
import Data.Default
import Auth

-- GHC 7.8 compatibility
import Data.Functor ((<$>))


-- | Greet checks you up on Google Plus, and prints your name and picture
--    to the site.
greet :: IO ()
greet = runR $ do
  -- Here we do a request with default parameters (def) to
  --  the version 1 api of the Google Plus API (urls can be found at
  --  Googles developer resources.) 
  response <- request "plus/v1/people/me" def

  -- Here we use lookupVal to extract some fields for presentation
  Just [name, pic] <- sequence <$> mapM (lookupVal response) [
    "result.displayName",
    "result.image.url"]

  -- Still in the request, we use liftIO to put elements on screen.
  liftIO . put $ "Hello " ++  name ++ "! <br />"
    ++ "You look like this: <br /><img src='"++ pic ++ "' />"


-- | Sets up Google API access and handle authentication 
main = withGAPI Auth.config -- our Auth config; see an example below
       $ \token -> case token of
  OA2Success {}           -> put (show token) >> greet
  -- If there is an error with the authentication, we print it here 
  OA2Error {errorMsg = e} -> put $ "I can't greet people with invalid "
                             ++ " access tokens :( (" ++ e ++ ")"

-- | This is an example of the config used to setup the Google API access
config = Config {
  -- | Your client id
  clientID = "", 
  -- | Your API keys (if any)
  apiKey = "",
  -- | Your scopes. We want to access a name and a profile picture, so
  --    'auth/plus.login' is sufficient
  scopes = "https://www.googleapis.com/auth/plus.login",
  -- | In our application, the token shouldn't be refreshed without the user
  --    knowing, so we won't use the immediate flag.
  immediate = False}



-- | Put looks for an <ul> with id=output, and creates one if none is found.
--    It then puts a new <li> element with the given string inside it. 
put :: String -> IO ()
put s = do
  item <- newElem "li" `with` ["innerHTML" =: pack s]  
  elem <- elemById "output"
  case elem of
    Just output -> output `appendChild` item
    Nothing     -> do output <- newElem "ul" `with` ["id" =: "output"]
                      output `appendChild` item
                      documentBody `appendChild` output

