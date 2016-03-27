module Haste.GAPI.Types where

-- | Resource versioning tag. Will be updated if the resource is updated
type ETag = String

-- | An e-mail address
type EMail = String

-- | Type of e-mail address. 
type EMailType = String

-- | An URL
type URL = String

-- | User id
type UserID = String

-- | Activity id
type ActivityID = String

-- | Type of Collection
type Collection = String

-- | Type of URL, like contributor (user is contributing to this site) or
--    website.
type URLType = String

-- | An image wrapped URL 
data Image = Image URL
           deriving Show
