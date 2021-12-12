module Util where

import Prelude

import Data.Array (intercalate)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty ((:|))
import Data.String as String
import Halogen as H
import Halogen.HTML.Properties as HP
import Types (NonNegativeInt(..))

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

classesS :: forall r p. String -> HP.IProp (class :: String | r) p
classesS = classes <<< String.split (String.Pattern " ")

asNNI :: Int -> Maybe NonNegativeInt
asNNI i
  | i < 0 = Nothing
  | otherwise = Just $ NonNegativeInt i

nn0 = NonNegativeInt 0 :: NonNegativeInt
nn1 = NonNegativeInt 1 :: NonNegativeInt
nn2 = NonNegativeInt 2 :: NonNegativeInt
nn3 = NonNegativeInt 3 :: NonNegativeInt
nn4 = NonNegativeInt 4 :: NonNegativeInt
nn5 = NonNegativeInt 5 :: NonNegativeInt
nn6 = NonNegativeInt 6 :: NonNegativeInt
nn7 = NonNegativeInt 7 :: NonNegativeInt
nn8 = NonNegativeInt 8 :: NonNegativeInt
nn9 = NonNegativeInt 9 :: NonNegativeInt