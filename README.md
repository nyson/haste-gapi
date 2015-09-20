Haste-GAPI
==========

Work in progress

Google API bindings for use with the [Haste compiler](http://haste-lang.org)

Usage
-----

This library depends on that the symbols Haste generates are avaliable at
execution of the script, not on load. For Haste-GAPI to work, it needs to
be compiled with the --onexec flag, as `haste --onexec CoolSite.hs`.

```haskell
data Config = Config {clientID  :: String,
                      apiKey    :: String,
                      scopes    :: String,
                      immediate :: Bool}

data OAuth2Token = OA2Success { accessToken :: String,
                                expiresIn   :: String,
                                state       :: String}
                 | OA2Error { errorMsg :: String,
                              state    :: String}
loadGAPI :: Config -> (OAuth2Token -> IO ()) -> IO ()
```

To make a successfull authentication, we apply the configuration and
a continuation to the `loadGapi` function. The contiuation will recieve
a `OAuth2Token` in which it can see if the authorization was successful.

```haskell
import Haste.GAPI

main = loadGAPI cfg $ \token -> case token of
     OA2Success {} -> putStrLn "We're in!"
     OA2Error {errorMsg = e} -> putStrLn $ "We're not in, and it's all "
	      		     	 ++ e ++ "'s fault!"
```

Your HTML needs to, as always, load the script from the Google API, and invoke
your generated Haste code by appending `onload=GAPILoader` to the js client
script. 

```html
<script type="text/javascript"
	src="https://apis.google.com/js/client.js?onload=GAPILoader">
</script>	     
```

