{-|
Module      : Haste.GAPI.Types
Description : Types used in different subsection APIs, like Google+
Copyright   : (c) Jonathan Skårstedt, 2016
License     : MIT
Maintainer  : jonathan.skarstedt@gmail.com
Stability   : experimental
Portability : Haste

Types for use by different APIs connecting to haste-gapi.
-}
module Haste.GAPI.Types where

import Haste (JSString)

-- | Resource versioning tag. Will be updated if the resource is updated
type ETag = JSString

-- | An e-mail address
type EMail = JSString

-- | Type of e-mail address.
type EMailType = JSString

-- | An URL
type URL = JSString

-- | User id
type UserID = JSString

-- | Activity id
type ActivityID = JSString

-- | Type of Collection
type Collection = JSString

-- | Type of URL, like contributor (user is contributing to this site) or
--    website.
type URLType = JSString

-- | An image wrapped URL
data Image = Image URL
           deriving Show
