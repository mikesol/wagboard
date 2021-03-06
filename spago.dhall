{ name = "wags.fm"
, dependencies =
  [ "ace"
  , "aff"
  , "aff-promise"
  , "arrays"
  , "behaviors"
  , "bifunctors"
  , "console"
  , "control"
  , "css"
  , "datetime"
  , "debug"
  , "dom-indexed"
  , "effect"
  , "either"
  , "event"
  , "exceptions"
  , "filterable"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "free"
  , "halogen"
  , "halogen-css"
  , "halogen-subscriptions"
  , "homogeneous"
  , "identity"
  , "integers"
  , "jit"
  , "js-timers"
  , "lcg"
  , "lists"
  , "math"
  , "maybe"
  , "newtype"
  , "nonbili-dom"
  , "nonempty"
  , "nullable"
  , "ordered-collections"
  , "parallel"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "psci-support"
  , "quickcheck"
  , "random"
  , "record"
  , "refs"
  , "simple-json"
  , "sized-vectors"
  , "strings"
  , "svg-parser-halogen"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "variant"
  , "wags"
  , "wags-lib"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
