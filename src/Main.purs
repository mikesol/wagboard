module Main where

import Prelude

import CSS (CSS, TimingFunction(..), fromString, sec)
import CSS.Hack.Animation (AnimationPlayState(..), animation, forwards, iterationCount, normalAnimationDirection)
import Components.Cycle as Cycle
import Components.Keyboard as Keyboard
import Config as C
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenM)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import JIT.Loader (makeLoader, Loader)
import Type.Proxy (Proxy(..))
import Types (KeyboardState(..), MainAction(..))
import Types as T
import Util (classes, nn0)

_cycle :: Proxy "cycle"
_cycle = Proxy

_keyboard :: Proxy "keyboard"
_keyboard = Proxy

type Slots =
  ( cycle :: forall query. H.Slot query Void T.NonNegativeInt
  , keyboard :: forall query. H.Slot query Void Unit
  )

foreign import hackishlyRemoveInitialSSR :: Effect Unit

showKeyboard :: CSS
showKeyboard = animation
  (fromString "showKeyboard")
  (sec 1.0)
  EaseInOut
  (sec 0.0)
  (iterationCount 1.0)
  normalAnimationDirection
  forwards
  ARunning

hideKeyboard :: CSS
hideKeyboard = animation
  (fromString "hideKeyboard")
  (sec 0.3)
  EaseInOut
  (sec 0.0)
  (iterationCount 1.0)
  normalAnimationDirection
  forwards
  ARunning

component :: forall q i o m. Loader -> MonadAff m => H.Component q i o m
component loader =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }
  where
  initialState _ = { showingKeyboard: Initial }

  render { showingKeyboard } =
    HH.div
      [
      ]
      [ HH.div
          [ classes
              [ "bg-gradient-to-r"
              , "from-purple-400"
              , "via-pink-500"
              , "to-red-500"
              , "w-screen"
              , "h-screen"
              , "absolute"
              , "top-0"
              , "left-0"
              , "flex"
              , "flex-col"
              ]

          ]
          [ HH.div
              [ classes [ "flex-grow", "overflow-scroll" ]
              , HE.onClick
                  ( const $ case showingKeyboard of
                      Showing -> HideKeyboard
                      _ -> ShowKeyboard
                  )
              ]
              [ HH.slot _cycle nn0 Cycle.component {} absurd ]
          , HH.div
              [ CSS.style do
                  case showingKeyboard of
                    Initial -> pure unit
                    Showing -> showKeyboard
                    Hidden -> hideKeyboard
              , classes [ "max-h-0" ]
              ]
              [ HH.slot _keyboard unit Keyboard.component {} absurd ]
          ]
      ]

  handleAction
    :: T.MainAction
    -> HalogenM T.MainState T.MainAction Slots o m Unit
  handleAction = case _ of
    ShowKeyboard -> H.modify_ _ { showingKeyboard = Showing }
    HideKeyboard -> H.modify_ _ { showingKeyboard = Hidden }

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  let loader = makeLoader C.loaderUrl
  runUI (component loader) unit body
