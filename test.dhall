let conf = ./spago.dhall

in    conf
    ⫽ { sources = conf.sources # [ "test/**/*.purs" ]
      , dependencies =
          conf.dependencies # [ "spec", "generics-rep", "js-timers" ]
      }
