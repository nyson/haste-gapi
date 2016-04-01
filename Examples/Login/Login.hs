{-# LANGUAGE OverloadedStrings #-}

import Haste.GAPI
import Data.Default
import qualified Auth

-- | A login example with haste-gapi
main = withGAPI Auth.config $ \t -> case t of
  OA2Error {errorMsg = e} -> putStrLn $ "Error lol " ++ e
  OA2Success {}           -> runR $ do
    response <- request "plus/v1/people/me" def
    Just name <- lookupVal response "result.displayName"
    liftIO $ putStrLn $ "Hello " ++ name ++ "!"
