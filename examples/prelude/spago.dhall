{ name =
    "purec-example-prelude"
, dependencies =
    [ "prelude" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
