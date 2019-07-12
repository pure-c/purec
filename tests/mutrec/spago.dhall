{ name =
    "purec-test-mutrec"
, dependencies =
    [] : List Text
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
