module Types where

import Prelude

import Data.Newtype (class Newtype)

newtype NonNegativeInt = NonNegativeInt Int

derive instance Newtype NonNegativeInt _
derive instance Eq NonNegativeInt
derive instance Ord NonNegativeInt

derive newtype instance Semiring NonNegativeInt

----- MAIN

type MainState = {}
data MainAction

------ CYCLE
type CycleState = {}
data CycleAction