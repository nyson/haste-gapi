{-# LANGUAGE OverloadedStrings #-}

{- A RequestM example that greets you if you have a Google+ profile! -}

import Prelude hiding (lookup)
import Haste
import qualified Haste.JSString as J 
import Haste.DOM.JSString (appendChild, with, (=:), newElem, elemById, documentBody)
import Haste.GAPI
import Data.Default
import Auth

-- GHC 7.8 compatibility
import Data.Functor ((<$>))

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


-- | Sets up Google API access and handle authentication
main = withGAPI Auth.config $ \token -> do
  success <- oa2Success token
  if success
    then runR token greet
    else do Just e <- errorMsg token
            put $ "Something went wrong: " `J.append` e


-- | Greet checks you up on Google Plus, and prints your name and picture
--    to the site.
greet :: RequestM ()
greet = do
  -- Here we do a request with default parameters (def) to
  --  the version 1 api of the Google Plus API (urls can be found at
  --  Googles developer resources.)

  -- `request' <path>` is making a request to a certain <path> without any
  --  parameters. If you want to make a request with parameters, use
  --  `request <path> [(<param key>, <param value>)]` instead.
  response <- request' "plus/v1/people/me"

  -- Here we use lookupVals to extract some fields for presentation.
  Just [name, pic] <- findVals response [
    "result.displayName",
    "result.image.url"]

  -- Still in the request, we use liftIO to put elements on screen.
  liftIO . put . J.concat $ ["Hello ", name, "! <br />",
                             "You look like this: <br />",
                             "<img src='", pic, "' />"]


-- | Put looks for an <ul> with id=output, and creates one if none is found.
--    It then puts a new <li> element with the given string inside it.
put :: JSString -> IO ()
put s = do
  item <- newElem "li" `with` ["innerHTML" =: s]
  elem <- elemById "output"
  case elem of
    Just output -> output `appendChild` item
    Nothing     -> do output <- newElem "ul" `with` ["id" =: "output"]
                      output `appendChild` item
                      documentBody `appendChild` output

