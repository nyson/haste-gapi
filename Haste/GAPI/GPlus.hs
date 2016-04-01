{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Haste.GAPI.GPlus
Description : Google+ API bindings for haste-gapi 
Copyright   : (c) Jonathan Skårstedt, 2016
License     : MIT
Maintainer  : jonathan.skarstedt@gmail.com
Stability   : experimental
Portability : Haste

Contains default function and result mappings for the Google+ API.
-}
module Haste.GAPI.GPlus (
  peopleGet,
  peopleSearch,
  peopleListByActivity,
  peopleList,
  -- | = Google+ datatypes:
  
  -- | == Google+ Person
  module Haste.GAPI.GPlus.Person
  
                        ) where


import Haste.GAPI.Types
import Haste.GAPI.GPlus.Person
import Haste.GAPI.Request
import Haste.JSString as J 
import Haste (JSString)

-- | Fetches a user by ID
peopleGet :: UserID -> RequestM (Result Person)
peopleGet uid = request' $ "plus/v1/people/" `append` uid

-- | Search after users given a query string
--
-- Optional parameters:
-- 
--   [@language@] @language@ to search with. See Google API documentation
--                for valid language codes              
-- 
--   [@maxResult@] Maximum number of people to include in the response.
--                 Acceptable values of @maxResult@ ranges between 1-50,
--                 default is 25.
--
--   [@pageToken@] Token used for pagination in large result sets. 
peopleSearch :: JSString -> Params -> RequestM [Result Person]
peopleSearch query ps = do
  r <- request "plus/v1/people" $ ("query", query) `pcons` ps
  r' <- get r "items"
  childs <- children r'
  case childs of
    Just childs' -> return childs'
    Nothing -> fail "peopleSearch: Child was not found!"
  
-- | List users by activity
--
-- Valid collections are "@plusoners@" and "@resharers@".
--
-- Optional parameters:
-- 
--   [@maxResult@] Maximum number of people to include in the response.
--                 Acceptable values of @maxResult@ ranges between 1-100,
--                 default is 20.
--
--   [@pageToken@] Token used for pagination in large result sets. 
peopleListByActivity :: ActivityID -> Collection -> Params
                        -> RequestM [Result Person]
peopleListByActivity actId col ps = do
  r <- request (J.concat ["plus/v1/activities/", actId, "/people/", col]) ps
  r' <- get r "items"
  childs <- children r'
  case childs of
    Just childs' -> return childs'
    Nothing -> fail "peopleSearch: Child was not found!"



-- | List people in a specific collection
--
-- Accepted collections are:
--
--   [@connected@] The list of visible people in the authenticated user's
--                 circles who also use the requesting app. This list
--                 is limited to users who made their app activities visible
--                 to the authenticated user.
--
--   [@visible@] The list of people who this user has added to one or more
--               circles, limited to the circles visible to the requesting
--               application.
-- 
-- Optional parameters:
-- 
--   [@maxResult@] Maximum number of people to include in the response.
--                 Acceptable values of @maxResult@ ranges between 1-100,
--                 default is 100.
--
--   [@orderBy@] The order to return people in. Valid inputs are
--               "@alphabetical@" and "@best@"
--
--   [@pageToken@] Token used for pagination in large result sets. 
peopleList :: UserID -> Collection -> Params -> RequestM [Result Person]
peopleList uid c ps = do
  r  <- request (J.concat ["plus/v1/people/", uid, "/people/", c]) ps
  r' <- get r "items"
  childs <- children r'
  case childs of
    Just childs' -> return childs'
    Nothing -> fail "peopleSearch: Child was not found!"
  







  
