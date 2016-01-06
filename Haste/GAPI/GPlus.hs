module Haste.GAPI.GPlus
       (Person(..),
        Name(..),
        Site(..),
        EMail(..), EMailType(..),
        URL(..), URLType(..),
        Image(..),
        Organization(..),
        AgeRange(..),
        Cover(..),

        peopleGet, peopleSearch, peopleListByActivity, peopleList
        
       )where

import Haste.GAPI.Types 
import Haste.GAPI.GPlus.Person
import Haste.GAPI.Request

-- | Fetches a user by ID
peopleGet :: UserID -> RequestM Person 
peopleGet = undefined

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
peopleSearch :: String -> Params -> RequestM [Person]
peopleSearch = undefined

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
                        -> RequestM [Person]
peopleListByActivity = undefined

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

{- 
Optional query parameters:
maxResults	unsigned integer
The maximum number of people to include in the response, which is used
for paging. For any response, the actual number returned might be less
than the specified maxResults. Acceptable values are 1 to 100, inclusive.
(Default: 100)

orderBy	string	The order to return people in. 
Acceptable values are:
"alphabetical": Order the people by their display name.
"best": Order people based on the relevence to the viewer.

pageToken	string	The continuation token, which is used to page
through large result sets. To get the next page of results, set this
parameter to the value of "nextPageToken" from the previous response.


-}
peopleList :: Collection -> Params -> RequestM [Person]
peopleList = undefined







  
