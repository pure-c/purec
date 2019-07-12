let mkPackage = ../../package-sets/mkPackage.dhall

let upstream = ../../package-sets/packages.dhall

let overrides = {=}

let additions = {=}

in  upstream ⫽ overrides ⫽ additions
