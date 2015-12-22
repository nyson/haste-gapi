-- {-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Haste.GAPI.Request.Result (
  liftIO,
  Result(..),
  FromResult,
  fromResult,
  dFromResult,
  get, has, deepGet,
  printResult 
  ) where 

import Haste.GAPI.Request.Types
import Haste.GAPI.Request.RequestM
import Haste.Foreign hiding (get, has)
import qualified Haste.Foreign as F 
import qualified Haste.JSString as J
import Haste.Prim (JSString)


import Control.Monad
import Control.Monad.IO.Class

-- TODO: Get some input into making better result desing?
newtype Result = Result {v :: JSAny}

instance FromAny Result where
  fromAny a = return $ Result a 

instance ToAny Result where
  toAny = v
  
class FromResult result where
  fromResult :: Result -> RequestM result

dFromResult :: FromAny a => Result -> RequestM a
dFromResult r = liftIO $ fromAny (v r)


debugPrintAny :: JSAny -> IO ()
debugPrintAny = ffi "(function(x) {console.debug(x)})"

printResult :: Result -> RequestM ()
printResult = liftIO . debugPrintAny . v


-- To lookup childerest:
-- >>> lookup obj "child.childer.childerest"
lookupAny :: JSAny -> JSString -> IO (Maybe JSAny)
lookupAny root i = foldM hasGet (Just root) $ J.match (J.regex "[^.]+" "g") i
  where hasGet :: Maybe JSAny -> JSString -> IO (Maybe JSAny)
        hasGet (Just parent) id = do h <- F.has parent id
                                     if h then Just <$> F.get parent id
                                       else pure Nothing

-- | like get, but will look at deeper objects given a `.` separated string.
deepGet :: (FromAny a) => Result -> String -> RequestM (Maybe a)
deepGet root keys = liftIO $ do l <- lookupAny (v root) (J.pack keys)
                                case l of Just a -> Just <$> fromAny a
                                          Nothing -> return Nothing 
  
-- | Gets a field from a result, on missing field RequestM will fail.
get :: (FromAny a) => Result -> String -> RequestM a
get r k = do exists <- has r k
             if exists
               then liftIO $ F.get (v r) (J.pack k)
               else fail   $ "'" ++ k ++ "' is not a valid key!"

-- | Gets a field, or a Nothing if there is no field with that name
getMaybe :: FromAny a => Result -> String -> RequestM (Maybe a)
getMaybe r k = do exists <- has r k
                  if exists
                    then do v <- liftIO $ F.get (v r) (J.pack k)
                            return $ Just v 
                    else return Nothing
                          
-- | Checks whether your result has a certain field
has :: Result -> String -> RequestM Bool
has r k = liftIO $ F.has (v r) (J.pack k)

-- | Checks if your type has all these values
hasAll :: Result -> [String] -> RequestM Bool
hasAll r keys = and <$> mapM (has r) keys

-- | Maps a couple of keys out to a new set of params
parp :: Result -> [String] -> RequestM Params
parp r keys = do v <- mapM (get r) keys
                 return . Params $ zip keys v

-- Some default instances 
instance FromResult String where fromResult = liftIO . fromAny . v
instance FromResult Bool   where fromResult = liftIO . fromAny . v
instance FromResult Int    where fromResult = liftIO . fromAny . v
instance FromResult Double where fromResult = liftIO . fromAny . v



