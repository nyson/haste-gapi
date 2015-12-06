Haste-GAPI
==========

Google API bindings for use with the [Haste compiler](http://haste-lang.org)

This is a work in progress, expect heavy API changes as I try to make an
API that's not horrible to use. I am in no way affiliated by Google.

What is this?
-----
This is a library to make use of the [Google API Client Library for JavasScript]
(https://developers.google.com/api-client-library/javascript/) in a Haskell
environment!

The library works by wrapping login and giving you a fancy type
to perform your requests in. This will ease chained requests that would
give large amounts of clutter in JavaScript, while giving you a better ability
to handle errors. If you only want to do a single or few requests and
feel that RequestM is quite heavy, you can also perform Request with Promises!

Your HTML doesn't even need to load the GAPI library itself, Haste-GAPI
handles that for you!


Usage
-----
While Haste-GAPI doesn't necessary needs it to work, I recommend you to
compile your Haste-GAPI applications with the --onexec flag, as in
`haste --onexec CoolSite.hs`. If you don't, you may notice a slightly slower
load time on sites with lots of resources.

To load your haste-gapi, just call this function:
```haskell
withGAPI :: Config -> (OAuth2Token -> IO ()) -> IO ()
```

This will load the Google API JavaScript library, and perform the given
function with a `OAuth2Token` as an argument. The type definition is below,
and it's used to determine the success of your authentication.

```haskell
data OAuth2Token = OA2Success { accessToken :: String,
                                expiresIn   :: String,
                                state       :: String}
                 | OA2Error { errorMsg :: String,
                              state    :: String}
```

If you want to login, you'll also have to have some login credentials,
which you ought to find at your [Google Developer Console permissions page](https://console.developers.google.com/permissions/).

This configuration takes a single API key (may be subject to change) along
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


main = loadGAPI cfg
       $ \token -> if oa2success token then putStrLn "We're in!!" 
	           else putStrLn $ "Something went wrong: " ++ oa2error token
```

Let's use what we know and perform a request! The code below will access
the Google+ APIs and ask who you are!

```haskell
import Haste
import Haste.GAPI
import Haste.GAPI.Request
import Haste.GAPI.GPlus

import Control.Monad.IO.Class
import Data.Default


main = withGAPI Auth.config $ \t -> case t of
  OA2Error {errorMsg = e} -> putStrLn $ "Error lol " ++ e
  OA2Success {}           -> rexec $ do
    response <- request "plus/v1/people/me" def
    person <- fromResult response
    liftIO $ putStrLn $ "Hello " ++ (show . name) person ++ "!"
```
