{ name =
    "purec-test-memory"
, dependencies =
    [] : List Text
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
