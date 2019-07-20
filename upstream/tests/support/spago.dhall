{ name =
    "support"
, dependencies =
    [ "arrays"
    , "assert"
    , "bifunctors"
    , "console"
    , "control"
    , "distributive"
    , "effect"
    , "either"
    , "foldable-traversable"
    , "functions"
    , "gen"
    , "generics-rep"
    , "identity"
    , "integers"
    , "invariant"
    , "lazy"
    , "lists"
    , "math"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "partial"
    , "prelude"
    , "proxy"
    , "refs"
    , "st"
    , "tailrec"
    , "tuples"
    , "type-equality"
    , "typelevel-prelude"
    , "unfoldable"
    , "unsafe-coerce"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
