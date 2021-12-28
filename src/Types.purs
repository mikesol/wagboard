module Types where

import Prelude

import Data.Newtype (class Newtype)

newtype NonNegativeInt = NonNegativeInt Int

derive instance Newtype NonNegativeInt _
derive instance Eq NonNegativeInt
derive instance Ord NonNegativeInt

derive newtype instance Semiring NonNegativeInt

----- MAIN

data KeyboardState = Initial | Showing | Hidden

type MainState =
  { showingKeyboard :: KeyboardState
  }

data MainAction = ShowKeyboard | HideKeyboard

------ CYCLE
type CycleState = {}
data CycleAction