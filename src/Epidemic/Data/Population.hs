{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Epidemic.Data.Population
-- Copyright: (c) 2021 Alexander E. Zarebski
-- License: MIT
--
-- Maintainer: Alexander E. Zarebski <aezarebski@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- This module defines some types and functions for working with identifiers,
-- persons, people, and populations.
--
--   * An 'Identifier' is used as a unique label for a 'Person',
--   * a 'Person' is a single individual,
--   * a group of 'People' is a collection of persons,
--   * and 'Population' is a typeclass for working with people that have some structure.
--

module Epidemic.Data.Population
  ( Person(Person)
  , People(People)
  , Population(..)
  , Identifier(Identifier)
  , asPeople
  , includesPerson
  , initialIdentifier
  , haveCommonPeople
  , newPerson
  , nullPeople
  , numPeople
  , personId
  , addPerson
  , addPersons
  , removePerson
  , personByteString
  ) where

import qualified Data.Aeson              as Json
import qualified Data.ByteString.Builder as BBuilder
import           Data.Coerce             (coerce)
import qualified Data.Set                as Set
import           GHC.Generics

-- | Class of types that can represent populations in an epidemic simulation.
class Population a where
  susceptiblePeople :: a -> Maybe People
  infectiousPeople :: a -> Maybe People
  removedPeople :: a -> Maybe People
  isInfected :: a -> Bool

-- | A type to hold an integer which is unique to each 'Person'.
newtype Identifier =
  Identifier Integer
  deriving (Show, Generic, Eq, Ord)

instance Json.FromJSON Identifier

instance Json.ToJSON Identifier

instance Enum Identifier where
  toEnum = Identifier . toInteger
  fromEnum (Identifier i) = fromInteger i

-- | A type to represent a single person in a group of 'People'
newtype Person =
  Person Identifier
  deriving (Show, Generic, Eq, Ord)

instance Json.FromJSON Person

instance Json.ToJSON Person

personId :: Person -> Identifier
personId = coerce

-- | A type to represent a population.
newtype People =
  People (Set.Set Person)
  deriving (Show, Eq, Generic)

instance Json.FromJSON People

instance Json.ToJSON People

-- | A list of persons as a people
asPeople :: [Person] -> People
asPeople persons = People $ Set.fromList persons

-- | Predicate for whether a person is one of the people
includesPerson :: People -> Person -> Bool
includesPerson (People persons) person = person `Set.member` persons

-- | Predicate for whether two sets of people have any members in common.
haveCommonPeople :: People -> People -> Bool
haveCommonPeople (People ps1) (People ps2) = not . Set.null $ Set.intersection ps1 ps2

-- | Predicate for whether there are any people
nullPeople :: People -> Bool
nullPeople (People persons) = Set.null persons

-- | The number of people
numPeople :: People -> Int
numPeople (People persons) = Set.size persons

-- | Add a person to a group of people if they are not already included.
addPerson :: Person -> People -> People
addPerson p people@(People ps) = People $ p `Set.insert` ps

-- | Add a person to a group of people if they are not already included.
addPersons :: [Person] -> People -> People
addPersons ps (People people) = People . Set.union people $ Set.fromList ps

-- | Remove a person from a group of people
removePerson :: Person -> People -> People
removePerson person (People persons) = People $ Set.filter (/= person) persons

-- | A bytestring builder for a person
personByteString :: Person -> BBuilder.Builder
personByteString (Person (Identifier n)) = BBuilder.charUtf8 'p' <> BBuilder.integerDec n

-- | An initial identifier.
initialIdentifier :: Identifier
initialIdentifier = Identifier 1

-- | A new person constructed from the given identifier and a new identifier.
newPerson :: Identifier -> (Person, Identifier)
newPerson id = (Person id, succ id)
