{-# LANGUAGE OverloadedStrings #-}
{- This is terribly undocumented and WILL be subject to refactoring. -}

module Haste.GAPI.GPlus where


import Haste.GAPI.Request.Result hiding (get, has)

import System.IO.Unsafe

import Haste.Foreign (fromAny, toAny, FromAny, ffi, JSAny, has)
import qualified Haste.Foreign as F

import Haste.GAPI.Types (ETag)


-- -- TODO: Export to a debug module?
-- debugAny :: JSAny -> IO ()
-- debugAny = ffi "(function(x) {console.debug(x);})"

emptyString :: IO JSAny
emptyString = ffi "(function() {return \"\";})"

get obj field = do
  hast <- has obj field
  if not hast then do -- debugAny obj
                      putStrLn $  "Field '" ++ show field ++ "' missing!"
                      emptyString >>= fromAny
    else F.get obj field
  

type EMail = String
type EMailType = String

type URL = String
type URLType = String

data Site = Site {label :: String, siteHref :: URL, siteType :: String}
          deriving Show

data Name = Name {formatted :: String,
                  familyName :: String,
                  givenName :: String,
                  middleName :: String,
                  honorificPrefix :: String,
                  honorificSuffix :: String}
instance Show Name where
  show n = givenName n ++ " " ++ familyName n

data Image = Image URL
             deriving Show

data Organization = Organization {orgName :: String,
                                  orgDepartment :: String,
                                  orgTitle :: String,
                                  orgType :: String,
                                  orgStartDate :: String,
                                  orgEndDate :: String,
                                  orgLocation :: String,
                                  orgDescription :: String,
                                  orgPrimary :: Bool}
                  deriving Show

data Place = Place {value :: String, primary :: Bool}
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
                          
data AgeRange = AgeRange {min :: Int, max :: Int}
              deriving Show
                         
data Person = Person {
  kind :: String,
  etag :: ETag,
  nickname :: String,
  occupation :: String,
  skills :: String,
  birthday :: String,
  gender :: String,
  emails :: [(EMail, EMailType)],
  urls :: [Site],
  objectType :: String,
  id :: String,
  displayName :: String,
  name :: Name,
  tagline :: String,
  braggingRights :: String,
  aboutMe :: String,
  relationshipStatus :: String,
  url :: String,
  image :: Image,
  organisations :: [Organization],
  placesLived :: [Place],
  isPlusUser :: Bool,
  language :: String,
  ageRange :: AgeRange,
  plusOneCount :: Int,
  circledByCount :: Int,
  verified :: Bool,
  cover :: Cover,
  domain :: String
  } deriving Show

instance FromAny CoverPhoto where
  fromAny a = CoverPhoto <$> get a "url" <*> get a "height" <*> get a "width"

instance FromAny CoverInfo where
  fromAny a = CoverInfo <$> get a "topImageOffset" <*> get a "leftImageOffset"

instance FromAny Site where
  fromAny a = Site <$> get a "label" <*> get a "value" <*> get a "type"
  
instance FromAny Name where
  fromAny a = Name <$> get a "formatted"
              <*> get a "familyName" <*> get a "givenName"
              <*> get a "middleName" <*> get a "honorificPrefix"
              <*> get a "honorificSuffix"

instance FromAny Image where
  fromAny a = Image <$> get a "url"

instance FromAny Organization where
  fromAny a = Organization <$> get a "name" <*> get a "department"
              <*> get a "title"     <*> get a "type"
              <*> get a "startDate" <*> get a "endDate"
              <*> get a "location"  <*> get a "description"
              <*> get a "primary"

instance FromAny Place where
  fromAny a = Place <$> get a "value" <*> get a "primary"

instance FromAny AgeRange where
  fromAny a = AgeRange <$> get a "min" <*> get a "max"
    
instance FromAny Cover where
  fromAny a = Cover <$> get a "layout"
              <*> get a "coverPhoto" <*> get a "coverInfo"

instance FromResult Person where
  fromResult = dFromResult

instance FromAny Person where
  fromAny r
    = do a <- get r "result"
         Person <$> get a "kind" 
           <*> get a "etag"           <*> get a "nickname"
           <*> get a "occupation"     <*> get a "skills"
           <*> get a "birthday"       <*> get a "gender"
           <*> get a "emails"         <*> get a "urls"
           <*> get a "objectType"     <*> get a "id"
           <*> get a "displayName"    <*> get a "name"
           <*> get a "tagline"        <*> get a "braggingRights"
           <*> get a "aboutMe"        <*> get a "relationshipStatus"
           <*> get a "url"            <*> get a "image"
           <*> get a "organizations"  <*> get a "placesLived"
           <*> get a "isPlusUser"     <*> get a "language"
           <*> get a "ageRange"       <*> get a "plusOneCount"
           <*> get a "circledByCount" <*> get a "verified"
           <*> get a "cover"          <*> get a "domain"
