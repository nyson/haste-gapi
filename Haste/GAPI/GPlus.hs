module Haste.GAPI.GPlus where

import Haste.GAPI.Types (ETag)

type EMail = String
type EMailType = String

type URL = String
type URLType = String

data Site = Site String URL SiteType deriving Show
data SiteType = OtherProfile | Contributor | Website | Other
                                                       deriving Show

data Name = Name {formatted :: String,
                  familyName :: String,
                  givenName :: String,
                  middleName :: String,
                  honorificPrefix :: String,
                  honorificSuffix :: String}
            deriving Show

data Image = Image URL
             deriving Show

data OrgType = Work | School
                      deriving Show
data Organisation = Org {orgName :: String,
                         orgDepartment :: String,
                         orgTitle :: String,
                         orgType :: OrgType,
                         orgStartDate :: String,
                         orgEndDate :: String,
                         orgLocation :: String,
                         orgDescription :: String,
                         orgPrimary :: Bool}
                    deriving Show

data Place = Place String Bool
             deriving Show

data Cover = Cover {layout :: String,
                    coverPhoto :: CoverPhoto,
                    coverInfo :: CoverInfo
                    }
             deriving Show

data CoverPhoto = CoverPhoto {
  covUrl :: URL,
  height :: Int,
  width :: Int }
                  deriving Show

data CoverInfo = CoverInfo {
  topImageOffset :: Int,
  leftImageOffset :: Int}
                 deriving Show
                          
data AgeRange = AgeRange Int Int 
              deriving Show
                         
data Person = Person {
  kind :: String,
  id :: String,
  displayName :: String,
  name :: Name,
  nickname :: String,
  birthday :: String,
  gender :: String,
  url :: String,
  image :: Image,
  aboutMe :: String,
  relationshipStatus :: String,
  urls :: [Site],
  organisations :: [Organisation],
  placesLived :: [Place],
  tagline :: String,
  objectType :: String,
  etag :: ETag,
  isPlusUser :: Bool,
  braggingRights :: String,
  plusOneCount :: Integer,
  circledByCount :: Integer,
  verified :: Bool,
  cover :: Cover,
  language :: String,
  ageRange :: AgeRange,
  emails :: [(EMail, EMailType)],
  domain :: String,
  skills :: String
  } deriving Show


  
