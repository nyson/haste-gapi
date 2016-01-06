{-# LANGUAGE OverloadedStrings #-}
{- This is terribly undocumented and WILL be subject to refactoring. -}

module Haste.GAPI.GPlus.Person where

import Haste.GAPI.Internals 
import Haste.GAPI.Request hiding (get, has)
import Haste.Foreign (fromAny, toAny, FromAny, ffi, JSAny, has)
import qualified Haste.Foreign as F
import Haste.GAPI.Types 

import Control.Applicative

-- | An URL of a Google+ user, 
data Site = Site {label :: String,
                  siteHref :: URL,
                  siteType :: URLType}
          deriving Show

-- | Name type of a Google+ user
data Name = Name {formatted :: String,
                  familyName :: String,
                  givenName :: String,
                  middleName :: String,
                  honorificPrefix :: String,
                  honorificSuffix :: String}
            
instance Show Name where
  show n = givenName n ++ " " ++ familyName n


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
  id :: UserID,
  displayName :: String,
  name :: Name,
  tagline :: String,
  braggingRights :: String,
  aboutMe :: String,
  relationshipStatus :: String,
  url :: String,
  image :: Image, -- TODO: Change to URL?
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
  fromAny a = CoverPhoto <$> getFail a "url" <*> getFail a "height" <*> getFail a "width"

instance FromAny CoverInfo where
  fromAny a = CoverInfo <$> getFail a "topImageOffset" <*> getFail a "leftImageOffset"

instance FromAny Site where
  fromAny a = Site <$> getFail a "label" <*> getFail a "value" <*> getFail a "type"
  
instance FromAny Name where
  fromAny a = Name <$> getFail a "formatted"
              <*> getFail a "familyName" <*> getFail a "givenName"
              <*> getFail a "middleName" <*> getFail a "honorificPrefix"
              <*> getFail a "honorificSuffix"

instance FromAny Image where
  fromAny a = Image <$> getFail a "url"

instance FromAny Organization where
  fromAny a = Organization <$> getFail a "name" <*> getFail a "department"
              <*> getFail a "title"     <*> getFail a "type"
              <*> getFail a "startDate" <*> getFail a "endDate"
              <*> getFail a "location"  <*> getFail a "description"
              <*> getFail a "primary"

instance FromAny Place where
  fromAny a = Place <$> getFail a "value" <*> getFail a "primary"

instance FromAny AgeRange where
  fromAny a = AgeRange <$> getFail a "min" <*> getFail a "max"
    
instance FromAny Cover where
  fromAny a = Cover <$> getFail a "layout"
              <*> getFail a "coverPhoto" <*> getFail a "coverInfo"

instance FromResult Person where
  fromResult = defFromResult

instance FromAny Person where
  fromAny r
    = do a <- F.get r "result"
         Person <$> getFail a "kind" 
           <*> getFail a "etag"           <*> getFail a "nickname"
           <*> getFail a "occupation"     <*> getFail a "skills"
           <*> getFail a "birthday"       <*> getFail a "gender"
           <*> getFail a "emails"         <*> getFail a "urls"
           <*> getFail a "objectType"     <*> getFail a "id"
           <*> getFail a "displayName"    <*> getFail a "name"
           <*> getFail a "tagline"        <*> getFail a "braggingRights"
           <*> getFail a "aboutMe"        <*> getFail a "relationshipStatus"
           <*> getFail a "url"            <*> getFail a "image"
           <*> getFail a "organizations"  <*> getFail a "placesLived"
           <*> getFail a "isPlusUser"     <*> getFail a "language"
           <*> getFail a "ageRange"       <*> getFail a "plusOneCount"
           <*> getFail a "circledByCount" <*> getFail a "verified"
           <*> getFail a "cover"          <*> getFail a "domain"
