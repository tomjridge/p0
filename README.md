# p0: a simple combinator parsing library

## Introductory example (CSV parser)

![1556706287321](README.assets/1556706287321.png)

## Quick links

* OCamldoc at <https://tomjridge.github.io/p0/> . 
* Examples of use in bin/p0_example.ml

## API fragment

![1556706452225](README.assets/1556706452225.png)

## Features

* Returns at most one result (the monad is the option monad, with None indicating a failed parse)
* Single file library (with a dependency on ocaml-re). 
* Good integration with ocaml-re