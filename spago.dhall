{ name =
    "purec"
, dependencies =
    [ "aff"
    , "avar"
    , "node-fs-aff"
    , "node-streams"
    , "corefn"
    , "spec"
    , "argonaut"
    , "nullable"
    , "console"
    , "effect"
    , "prelude"
    , "free"
    , "psci-support"
    , "debug"
    , "node-process"
    , "node-child-process"
    , "foreign"
    , "node-readline"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
