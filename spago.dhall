{ name =
    "invariant-generic"
, dependencies =
    [ "record", "invariant", "tuples", "typelevel-prelude" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
