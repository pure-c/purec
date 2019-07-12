{ name =
    "purec-test-prelude"
, dependencies =
    [ "prelude" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
