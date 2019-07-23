{ name =
    "purec-test-datacons"
, dependencies =
    [] : List Text
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
