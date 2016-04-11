{-# LANGUAGE OverloadedStrings#-}
module Haste.GAPI.Token (
  OAuth2Token, 
  oa2Success, state,
  -- | == For successful tokens
  accessToken, expiresIn, 
  -- | == For erroneous tokens
  errorMsg

  )where

import Haste (JSString)
import Haste.Foreign
import Control.Monad

-- | OAuth2Token, the authentication tokens used by the Google API.
data OAuth2Token = OA2 JSAny

instance FromAny OAuth2Token where
  fromAny any = OA2 <$> return any

instance ToAny OAuth2Token where
  toAny (OA2 a) = a 

-- | OAuth2 successful check on a JSAny. Can backfire and should not be
--    exported.
oa2Success' :: JSAny -> IO Bool
oa2Success' tok = hasAll tok ["access_token", "state", "expires_in"]

-- | Returns true if the token represents a successful authentication
{-# INLINE oa2Success #-}
oa2Success :: OAuth2Token -> IO Bool 
oa2Success (OA2 tok) = oa2Success' tok

-- | Will return the access token given that the token is successful
accessToken :: OAuth2Token -> IO (Maybe JSString)
accessToken (OA2 tok) = do
  successful <- oa2Success' tok 
  if successful
    then Just <$> get tok "access_token"
    else return Nothing

-- | Retrieves an expiration time of a successful token 
expiresIn :: OAuth2Token -> IO (Maybe JSString) 
expiresIn (OA2 tok) = do
  successful <- oa2Success' tok
  if successful
    then Just <$> get tok "expires_in"
    else return Nothing 

-- | Retrieves an error message from a failing authentication
errorMsg :: OAuth2Token -> IO (Maybe JSString)
errorMsg (OA2 tok) = do
  erroneous <- not <$> oa2Success' tok
  if erroneous
    then fmap Just $ get tok "error" >>= fromAny 
    else return Nothing

-- | Retrieves the state of a token 
state :: OAuth2Token -> IO JSString
state (OA2 tok) = get tok "state" >>= fromAny

