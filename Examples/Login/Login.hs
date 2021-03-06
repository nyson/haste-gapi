{-# LANGUAGE OverloadedStrings #-}

import Haste.GAPI
import qualified Haste.JSString as J
import Control.Monad (when)
import Data.Default
import qualified Auth

import qualified Haste.Foreign as F

-- | A login example with haste-gapi
main = withGAPI Auth.config $ \token -> do
  describeToken token
  successful <- oa2Success token
  when successful $ runR token you

-- | Performs a request to the Google+ API and asks who you are
you :: RequestM ()
you = do
  response <- request' "plus/v1/people/me"
  Just name <- findVal response "result.displayName"
  liftIO . putStrLn $ "Hello " ++ name ++ "!"


-- | Describes a token for authorisation
describeToken :: OAuth2Token -> IO ()
describeToken tok = do
  let log = putStrLn . J.unpack . J.concat
  successful <- oa2Success tok

  if successful
    then do
      Just exp   <- expiresIn tok
      Just t     <- accessToken tok
      st         <- state tok
      log ["Your successful token is '", t, "' and expires in ", exp,
           " seconds. The state is '", st, "'"]

    else do
      Just err <- errorMsg tok
      st       <- state tok
      log ["Your auth fails because of \"", err,
           "\"; the state is '", st, "'"]
