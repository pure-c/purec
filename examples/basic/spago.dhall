{ name =
    "purec-example-basic"
, dependencies =
    [] : List Text
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
