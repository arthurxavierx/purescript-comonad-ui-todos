
{ name = "comonad-ui-todos"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "free"
  , "freet"
  , "functors"
  , "prelude"
  , "psci-support"
  , "react-dom"
  , "simple-json"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
