{ name = "purec-test-prelude"
, dependencies = [ "prelude" ]
, packages = ../../package-sets/packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
