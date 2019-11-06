{ name =
    "purec-test-effects"
, dependencies =
    [ "prelude", "effect" ]
, packages =
    ../../package-sets/packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
