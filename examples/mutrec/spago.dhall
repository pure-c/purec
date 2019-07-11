{ name =
    "purec-example-mutrec"
, dependencies =
    [] : List Text
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
