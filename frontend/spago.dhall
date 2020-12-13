{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "hs-forum"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "argonaut-generic"
  , "console"
  , "datetime"
  , "effect"
  , "generics-rep"
  , "psci-support"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  ]
, packages = ./packages.dhall
, sources = [ "purs/src/**/*.purs", "purs/test/**/*.purs" ]
}
