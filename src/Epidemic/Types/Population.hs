{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Epidemic.Types.Population
  ( Person(Person)
  , People(People)
  , Identifier(Identifier)
  , asPeople
  , includesPerson
  , haveCommonPeople
  , nullPeople
  , numPeople
  , addPerson
  , removePerson
  , personByteString
  ) where

import qualified Data.Aeson as Json
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BBuilder
import Data.ByteString.Internal (c2w)
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import GHC.Generics

newtype Identifier =
  Identifier Integer
  deriving (Show, Generic, Eq)

instance Json.FromJSON Identifier

instance Json.ToJSON Identifier

instance Csv.ToField Identifier where
  toField (Identifier n) = Csv.toField n

instance Csv.FromField Identifier where
  parseField f = Identifier <$> (Csv.parseField f :: Csv.Parser Integer)

newtype Person =
  Person Identifier
  deriving (Show, Generic, Eq)

instance Json.FromJSON Person

instance Json.ToJSON Person

instance Csv.ToField Person where
  toField (Person n) = Csv.toField n

instance Csv.FromField Person where
  parseField f =
    Person . Identifier <$> (Csv.parseField f :: Csv.Parser Integer)

newtype People =
  People (V.Vector Person)
  deriving (Show, Eq, Generic)

instance Json.FromJSON People

instance Json.ToJSON People

instance Csv.ToField People where
  toField (People persons) =
    B.intercalate ":" $ V.toList $ V.map Csv.toField persons

instance Csv.FromField People where
  parseField f =
    People . V.fromList <$> mapM Csv.parseField (B.split (c2w ':') f)

-- | A list of persons as a people
asPeople :: [Person] -> People
asPeople persons = People $ V.fromList persons

-- | Predicate for whether a person is one of the people
includesPerson :: People -> Person -> Bool
includesPerson (People persons) person = V.elem person persons

-- | Predicate for whether two sets of people have any members in common.
haveCommonPeople :: People -> People -> Bool
haveCommonPeople (People ps1) (People ps2) = V.any (\p -> V.elem p ps2) ps1

-- | Predicate for whether there are any people
nullPeople :: People -> Bool
nullPeople (People persons) = V.null persons

-- | The number of people
numPeople :: People -> Int
numPeople (People persons) = V.length persons

-- | Add a person to a group of people
addPerson :: Person -> People -> People
addPerson person (People persons) = People $ V.cons person persons

-- | Remove a person from a group of people
removePerson :: Person -> People -> People
removePerson person (People persons) = People $ V.filter (/= person) persons

-- | A bytestring builder for a person
personByteString :: Person -> BBuilder.Builder
personByteString (Person (Identifier n)) = BBuilder.integerDec n
