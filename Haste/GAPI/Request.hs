{-# LANGUAGE OverloadedStrings #-}
module Haste.GAPI.Request ( Promise,
                            RequestM (..),
                            Reason (..),
                            Response (..), 
                            Params (..),
                            Request (..),
                            Path (..),
                            gapiError, 
                            rawRequest,
                            withRequest,
                            cRequest,
                            rexec,
                            request
                            
                          ) where

import Haste.GAPI.Request.Promise
import Haste.GAPI.Request.RequestM
import Haste.GAPI.Request.Types
import Haste.GAPI.Request.Raw
import Haste.GAPI.Request.Result


import Haste.Foreign
import Haste.Concurrent 

import Control.Monad

-- | Applies request and executes the given Promise.
withRequest :: Request -> Promise -> IO ()
withRequest r p = do re <- jsCreateRequest (path r) (toAny $ params r)
                     applyPromise re p

-- | Executes a request, blocking while waiting for the result and then
-- return the finished equation
cRequest :: Request -> CIO JSAny
cRequest r = do
  v <- newEmptyMVar
  liftIO . withRequest r
    $ Promise (concurrent . putMVar v)
    $ \_ -> putStrLn "Error"
  takeMVar v
    
-- | Executes a request
rexec :: RequestM () -> IO ()
rexec = concurrent . void . unR

-- | Creates a request
request :: Path -> Params -> RequestM Result
request p params = customRequest . rawRequest p $ params

customRequest :: Request -> RequestM Result
customRequest req = do
  v <- newEmptyMVar
  liftConc . fork . liftIO $ do
    resp <- jsCreateRequest (path req) (toAny $ params req)
    applyPromise resp $ Promise (concurrent . putMVar v . Right . Result)
      (\r -> concurrent . putMVar v . Left $ "Request error: " ++ show req)
  Req $ takeMVar v

-- | Fetches a value from a response. The return type must have
--   a ToAny instance
fetch :: FromAny a => Response -> String -> RequestM a
fetch = undefined

-- | Puts a value in an object. Must have a ToAny instance
put :: ToAny a => Response -> String -> a -> RequestM ()
put = undefined

-- | Takes fields with the given identifiers from response and stores
--   it in a new Params under the same name.
fields :: [String] -> Response -> RequestM Params
fields = undefined

-- | Takes fields with the first element of each tuple and puts them
--   in a new Params with the second element of that tuple as identifier
fields' :: [(String, String)] -> Response -> RequestM Params
fields' = undefined

merge :: Params -> Params -> Params
merge (Params xs) (Params ys) = Params $ xs ++ ys 
