Haste-GAPI
==========

![Build Status](https://travis-ci.org/nyson/haste-gapi.svg?branch=master)

Google API bindings for use with the [Haste compiler](http://haste-lang.org)

This is a work in progress, and there may be API changes while I try to design
an API that's not horrible to use. I am in no way affiliated by Google.

What is this?
-----
This is a library to make use of the [Google API Client Library for JavasScript](https://developers.google.com/api-client-library/javascript/) 
in a Haskell environment!

The library works by wrapping login and giving you a fancy type
to perform your requests in. This will ease chained requests that would
give large amounts of clutter in JavaScript, while giving you a better ability
to handle errors. If you only want to do a single or few requests and
feel that RequestM is quite heavy, you can also perform Request with Promises!

Your HTML doesn't even need to load the GAPI library itself, Haste-GAPI
handles that for you!


Usage
-----
First off, set up an account at the
 [Google Developer Console](https://console.developers.google.com/)
 if you haven't already done so.

While Haste-GAPI doesn't necessary needs it to work, I recommend you to
compile your Haste-GAPI applications with the `--onexec` flag, as in
`haste --onexec CoolSite.hs`. If you don't, you may notice slightly longer
load time on sites with lots of resources as the Google API hooks download
first after the DOM is loaded.

To load your haste-gapi, just call this function:
```haskell
withGAPI :: Config -> (OAuth2Token -> IO ()) -> IO ()
```
This will load the Google API JavaScript library, and perform the given
function with a `OAuth2Token` as an argument. The token is needed to perform
requests within the Google API.

If you want to login, you'll also have to have some login credentials,
which you ought to find at your [Google Developer Console permissions page](https://console.developers.google.com/permissions/).

This configuration takes a single API key along
with your clientID and scopes.

```haskell
data Config = Config {clientID  :: String,
                      apiKey    :: String,
                      scopes    :: String,
                      immediate :: Bool}
```

To perform a successfull authentication, we apply the configuration and
a continuation to the `withGapi` function. We will then recieve
an `OAuth2Token` in which we can see if the authorization was successful.

```haskell
import Haste.GAPI

-- | Retrieving a token (config is defined elsewhere)
main = withGAPI Auth.config $ \token -> do
     success <- oa2Success token 
     if success
        then putStrLn $ "We're in!"
  	else do error <- errorMsg token 
          	putStrLn $ "There was an error: " ++ error
  	
```

### Making a request with RequestM (rexec)!

Let's use what we know and perform a request! The code below will access
the Google+ APIs and ask who you are!

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Haste.GAPI
import Data.Default

-- | A login example with haste-gapi (config is defined elsewhere)
main = withGAPI Auth.config $ \token -> runR token $ do
    response <- request "plus/v1/people/me" def
    Just name <- lookupVal response "result.displayName"
    liftIO . putStrLn $ "Hello " ++ name ++ "!"
```
