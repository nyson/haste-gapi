{-# LANGUAGE OverloadedStrings #-}
module Haste.GAPI.Promise where

import qualified Haste.JSString as J

import Haste.Foreign
import Haste

import Control.Monad
import Data.Maybe
import qualified Data.List as List

type Response = JSAny
type Reason = JSAny 
data Promise = Promise (Response -> IO ()) (Reason -> IO ())
             | Callback (Response -> IO ())

{- Some kind of MVar so represent the result maybe? -}
instance ToAny Promise where
  toAny (Promise thn err) = toObject [("then", toAny thn),
                                      ("error", toAny err)]
  toAny (Callback cbk) = toObject [("then", toAny cbk), ("error", toAny gapiError)]

applyPromise :: JSAny -> Promise -> IO ()
applyPromise = ffi "function(action, p) {action.then(p.then, p.error);}"

-- | Default error handler in promises
gapiError :: (JSString -> IO ()) -> Reason -> IO ()
gapiError action reason
  = do msg <- lookupAny reason "result.error.message"
       case msg of
        Just m -> do str <- fromAny m
                     action str
        Nothing -> putStrLn "Malformed response!"


-- | Looking up a successor of an object by a deep id.
-- Say you have a JavaScript object: 
--
-- >>> obj = {child: {childer: {childerest: "Hello sir!"}}};
--
-- To lookup childerest:
-- >>> lookup obj "child.childer.childerest"
lookupAny :: JSAny -> JSString -> IO (Maybe JSAny)
lookupAny root i = foldM hasGet (Just root) $ J.match (J.regex "[^.]+" "g") i
  where hasGet :: Maybe JSAny -> JSString -> IO (Maybe JSAny)
        hasGet (Just parent) id = do h <- has parent id
                                     if h then Just <$> get parent id
                                       else pure Nothing

-- | Tries to find an element by deep id, or returns the deepest search point
lookupAny' :: JSAny -> JSString -> IO (Either JSAny JSString)
lookupAny' root ids = do
  (node, prevs) <- foldM hasGet (Just root, []) $ J.match (J.regex "[^.]+" "g") ids
  case node of
   Just node' -> return $ Left node'
   Nothing ->
     let clean = pure . Right . J.concat . reverse . List.intersperse (J.pack ".")
     in clean prevs
  
  where hasGet :: (Maybe JSAny, [JSString])
                  -> JSString
                  -> IO (Maybe JSAny, [JSString])
        hasGet (Nothing, prevs) id = return (Nothing, id:prevs)
        hasGet (Just node, prevs) id = do
          h <- node `has` id
          if h then do node' <- get node id
                       return (node', id:prevs)
            else return (Nothing, id:prevs)

                                               

                       
