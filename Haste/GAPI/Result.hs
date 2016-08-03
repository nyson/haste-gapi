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
  has, hasAll,
  get, getVal,
  find, findVal, findVals,
  children, childrenVal,
  val,
  parp,
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

-- | Checks if a Result has a certain field
has :: Result a -> JSString -> RequestM Bool
has (Result res) key = liftIO $ F.has res key

-- | Checks if a Result has several different fields. Will return true only
--    if all these fields exists.
hasAll :: Result a -> [JSString] -> RequestM Bool
hasAll res keys = and <$> mapM (has res) keys

-- | Fetches a data field from a GAPI Result
get :: Result a -> JSString -> RequestM (Result b)
get (Result r) key = liftIO $ do
  h <- F.has r key
  if h then Result <$> F.get r key
    else fail $ "Child '" ++ J.unpack key ++ "' was not found."

-- | Gets a value instead of a result container from a result
getVal :: F.FromAny any => Result a -> JSString -> RequestM any
getVal r key = get r key >>= val

-- | Fetches a result by a deep index
--   Example:
--
--   @
--     findVal res "a.b.c"
--   @
--
--   This will look up`(a.b.c)` where `a` is a field of
--    `res`, `b` is a field of `a`, and `c` is a field of `b`.
find :: Result a -> JSString -> RequestM (Maybe (Result b))
find res keys = liftIO $ fmap Result <$> F.lookupAny (rawR res) keys

-- | Fetches a value from a deep index
findVal :: F.FromAny any => Result a -> JSString -> RequestM (Maybe any)
findVal res keys = do
  v <- find res keys
  case v of
    Just v' -> Just <$> val v'
    Nothing -> return Nothing

-- | Find all values in a result and returns
findVals :: F.FromAny any => Result a -> [JSString] -> RequestM (Maybe [any])
findVals r keys = sequence <$> mapM (findVal r) keys

-- | Checks if a fromAny is an array
isArray :: JSAny -> IO Bool
isArray = F.ffi "(function(a) {return a.prop && a.prop.constructor === Array;})"

-- | Transforms a result which represents an array into a list of child
--    elements
children :: Result a -> RequestM (Maybe [Result b])
children (Result res) = do
  arr <- liftIO $ isArray res
  if arr
    then do
      childs <- liftIO $ children' res
      return . Just $ map Result childs
    else
      return Nothing
  where
    -- | Transforms a JSAny representing a list into a list of its children
    children' :: JSAny -> IO [JSAny]
    children' = F.fromAny

-- | Same as children, but will retrieve workable data instead of result
--    wrappings
childrenVal :: F.FromAny any => Result a -> RequestM (Maybe [any])
childrenVal r = do
  childs <- children r
  case childs of
    Just cs -> Just <$> mapM val cs
    Nothing -> return Nothing


-- | Fetches the value of the result directly.
val :: F.FromAny any => Result a -> RequestM any
val = liftIO . F.fromAny . rawR

-- | `parp`, or /__par__ameter __p__airs/, Takes a result and a list of keys
--   and maps these values to Params. Example:
--
--   @
--     result <- request apiPath firstParams
--     params <- parp result ["id", "stuffs"]
--     request anotherApiPath params
--   @
--
--  Given that the result contained fields __id = "uid19530"__ and
--  __stuffs = "I am  a fond lover of hats"__, `params` will
--  now be a list as below, making it easy to chain parameters between
--  requests.
--
--  prop> params = [("id", "uid19530"), ("stuff", "I am a fond lover of hats")]
parp :: Result a -> [JSString] -> RequestM (Maybe [Param])
parp r keys = do
  exists <- hasAll r keys
  if not exists
    then return Nothing
    else do vals <- mapM (\key -> get r key >>= val) keys
            return . Just $ zip keys vals


