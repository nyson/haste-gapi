-- {-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Haste.GAPI.Request.Result (
  liftIO, Result(..), FromResult, fromResult
                                 ) where 
import Haste.GAPI.Request.RequestM
import Haste.Foreign

import Control.Monad.IO.Class

newtype Result = Result {v :: JSAny}

class FromResult result where
  fromResult :: Result -> RequestM result

-- Some default instances 
instance FromResult String where fromResult = liftIO . fromAny . v
instance FromResult Bool   where fromResult = liftIO . fromAny . v
instance FromResult Int    where fromResult = liftIO . fromAny . v
instance FromResult Double where fromResult = liftIO . fromAny . v



