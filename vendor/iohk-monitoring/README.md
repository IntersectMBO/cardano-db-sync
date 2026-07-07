# iohk-monitoring-framework

[![Build status](https://badge.buildkite.com/1cc7939a1fed4972c15b8f87d510e0404b0eb65d73cfd1e30b.svg?branch=master)](https://buildkite.com/input-output-hk/iohk-monitoring-framework)
[![Coverage Status](https://coveralls.io/repos/github/input-output-hk/iohk-monitoring-framework/badge.svg?branch=master)](https://coveralls.io/github/input-output-hk/iohk-monitoring-framework?branch=master)

This framework provides logging, benchmarking and monitoring.

## documentation

Documentation of the [source code and tests](docs/IOHK-Monitoring.pdf) in PDF format. Please, download the PDF file and open in external viewer. It contains links that make it easy to navigate in the source code. Those links are not active in the online viewer.

Presentations and more documentation is available from our [docs](https://input-output-hk.github.io/iohk-monitoring-framework/) section.

## module dependencies

![Overview of modules](docs/OverviewModules.png)

## building and testing

`cabal new-build iohk-monitoring`

`cabal new-test pkg:iohk-monitoring:tests`

## examples
https://github.com/input-output-hk/iohk-monitoring-framework/edit/master/README.md
Some examples are available in the directory `examples`:
* `simple`  -  run with `cabal new-run example-simple`
* `complex`  -  run with `cabal new-run example-complex`
* `performance` - run with `cabal new-run example-performance`

These showcase the usage of this framework in an application. The *complex* example includes `EKGView` (http://localhost:12789) and the configuration editor (http://localhost:13789).

![Edit runtime configuration](docs/ConfigEditor.png)

## development

* `cabal new-build` and `cabal new-test`
* `ghcid -c "cabal new-repl"` watches for file changes and recompiles them immediately
* `liquid --ghc-option=-XOverloadedStrings --prune-unsorted src/Cardano/BM/*.lhs` verify top modules in iohk-monitoring using LiquidHaskell
