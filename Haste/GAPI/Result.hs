{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Haste.GAPI.Result
Description : Managing Results within the Google API
Copyright   : (c) Jonathan Skårstedt, 2016
License     : MIT
Maintainer  : jonathan.skarstedt@gmail.com
Stability   : experimental
Portability : Haste

This module contains functions to work on results from requests to the Google
API. 
-}


module Haste.GAPI.Result (
  Result(),
  Raw(..), 
  get, has, hasAll, 
  lookupVal, lookupResult,
  val, valOf, valMaybe,
  parp, children,
  toResult 
  ) where

import Haste
import qualified Haste.Foreign as F 
import qualified Haste.JSString as J

-- GHC 7.8 compatibility
import Data.Functor ((<$>))

import Haste.GAPI.Request.Types
import Haste.GAPI.Request.RequestM

-- | Result mapping. The Result takes a parameter to act as a phantom type
--    on which kind of object you're currently working on.
newtype Result a = Result {rawR :: F.JSAny}


-- | Transforms a JSAny to a Result
toResult :: JSAny -> Result a
toResult = Result 

-- | Default result parameter.
data Raw 

-- | Fetches a data field from a GAPI Result 
get :: Result a -> JSString -> RequestM (Result Raw)
get (Result r) key = liftIO $ do
  h <- F.has r key
  if h then Result <$> F.get r key
    else fail $ "Child '" ++ J.unpack key ++ "' was not found."

-- | Looks up a value by a deep index
--   Example:
-- 
--   @
--     rLookup res "a.b.c"
--   @
--
--   This will look up`(a.b.c)` where `a` is a field of
--    `res`, `b` is a field of `a`, and `c` is a field of `b`.
lookupVal :: F.FromAny any => Result a -> JSString -> RequestM (Maybe any)
lookupVal res keys = liftIO $ do
  v <- F.lookupAny (rawR res) keys
  case v of
    Just a -> Just <$> F.fromAny a
    Nothing -> return Nothing 

-- | Fetches a result from a deep index
lookupResult :: Result a -> JSString -> RequestM (Maybe (Result Raw))
lookupResult res keys
  = liftIO $ fmap Result <$> F.lookupAny (rawR res) keys

-- | Checks if a Result has a certain field
has :: Result a -> JSString -> RequestM Bool
has (Result res) key = liftIO $ F.has res key

-- | Checks if a Result has several different fields. Will return true only
--    if all these fields exists. 
hasAll :: Result a -> [JSString] -> RequestM Bool
hasAll res keys = and <$> mapM (has res) keys

-- | Fetches a value from a field, in contrast to a result.
--    Field must contain a value compatible with fromAny
--    Terminates with an error if the field cannot be found.
val :: F.FromAny any => Result a -> JSString -> RequestM any 
val r key = liftIO $ F.get (rawR r) key >>= F.fromAny

-- | Fetches the value of the result directly.
valOf :: F.FromAny any => Result a -> RequestM any
valOf = liftIO . F.fromAny . rawR

-- | Same as `val`, but will return Nothing if there is no such field.
valMaybe :: F.FromAny any => Result a -> JSString -> RequestM (Maybe any)
valMaybe (Result res) key = liftIO $ do
  exists <- F.has res key
  if exists then Just <$> (F.get res key >>= F.fromAny)
    else return Nothing

-- | Takes a result and a list of keys and maps these values to Params.
parp :: Result a -> [JSString] -> RequestM (Maybe Params)
parp r keys = do
  exists <- hasAll r keys
  if not exists
    then return Nothing
    else do vals <- mapM (val r) keys
            return . Just . Params $ zip keys vals


-- | Transforms a JSAny representing a list into a list of its children
children' :: JSAny -> IO [JSAny]
children' = F.fromAny

-- | Checks if a fromAny is an array 
isArray :: JSAny -> IO Bool
isArray = F.ffi "(function(a) {\
\return a.prop \
\&& a.prop.constructor === Array;\
\})"

-- | Transforms a result which represents an array into a list of child
--    elements
children :: Result a -> RequestM (Maybe [Result b])
children (Result res) = do
  arr <- liftIO $ isArray res
  if arr 
    then do 
      childs <- liftIO $ children' res
      return . Just $ map Result childs
    else return Nothing
