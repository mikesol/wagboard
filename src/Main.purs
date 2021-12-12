module Main where

import Prelude

import Components.Cycle as Cycle
import Config as C
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenM)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import JIT.Loader (makeLoader, Loader)
import Type.Proxy (Proxy(..))
import Types as T
import Util (classes, nn0)

_cycle :: Proxy "cycle"
_cycle = Proxy

type Slots =
  ( cycle :: forall query. H.Slot query Void T.NonNegativeInt
  )

foreign import hackishlyRemoveInitialSSR :: Effect Unit

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
  initialState _ = {}

  render _ =
    HH.div
      [ classes
          [ "w-screen"
          , "h-screen"
          , "bg-gradient-to-r"
          , "from-purple-400"
          , "via-pink-500"
          , "to-red-500"
          ]
      ]
      [ HH.slot _cycle nn0 Cycle.component {} absurd
      ]

  handleAction
    :: T.MainAction
    -> HalogenM T.MainState T.MainAction Slots o m Unit
  handleAction = mempty

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  let loader = makeLoader C.loaderUrl
  runUI (component loader) unit body
