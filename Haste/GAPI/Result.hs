module Haste.GAPI.Result (
  Result(..),
  get, has,
  lookupVal, lookupResult,
  val, valOf, valMaybe,
  parp
  
                         )
       where

import Haste
import qualified Haste.Foreign as F 
import qualified Haste.JSString as J

import Haste.GAPI.Request.Types
import Haste.GAPI.Request.RequestM

-- | Result mapping 
newtype Result a = Result {rawR :: F.JSAny}

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
--    Terminates with an error if the field cannot be found
val :: F.FromAny any => Result a -> JSString -> RequestM any 
val r key = liftIO $ F.get (rawR r) key >>= F.fromAny

valOf :: F.FromAny any => Result a -> RequestM any
valOf = liftIO . F.fromAny . rawR

-- | Same as `val`, but will return Nothing if there is no such field
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
