[![Build](https://github.com/typedb-osi/typedb-client-haskell/actions/workflows/main.yml/badge.svg?branch=master)](https://github.com/typedb-osi/typedb-client-haskell/actions/workflows/main.yml)

# TypeDB Client for Haskell (INCUBATING)

This is a TypeDB client implementation for Haskell.
It uses AwakeSecurity's gRPC library to implement the TypeDB Protocol.
There are three versions planned (all within this package):

- Type-Safe-Client: Haskell code compiled along with a TypeDB schema 
  to support strongly typed queries and other operations for a specific Schema
- High-Level-Client: A nice API for the TypeDB Protocol and most needs.
- Low-Level-Client: a usable wrapper around the generated gRPC Code.
  
## Building
This project uses Nix. This is only partly wanted because it strongly restrict interaction
with the "standard" way of building Haskell using stack/cabal. However, the underlying
gRPC library needs to be built with nix right now. Once it is possible to build the library
without nix, building with cabal solely will be possible.

To get a development environment use 
```sh
> nix-shell
```

Building from within the dev env:
```sh
> cabal build
```

to build the project in one command:
```sh 
> nix-shell --command "cabal update && cabal build"
```

 ## Status

the haskell client successfully performed its' first action:
- querying the available DBs,
- creating a new one
- initializing it with a schema 
- committing the whole thing
- closing the connection

query DSL compiles to proper query strings

still a long road to go; but nevertheless a small victory ^^

 *Beware:* Currently, the core code is messy, there is not much testing and there are
 still design choices planned that are subject to change if I see fit. 
 Enter at own risk ;)
 
 ## Contributing
 
 Contributions in form of pull requests and issues are welcome. 
 If you plan to contribute larger parts, question the design choices or anything
 of that magnitude (or if you simply want some human contact) please visit the 
 devleopment channel [on discord in the typedb discord server](https://discord.com/channels/665254494820368395/837321869937213450)

 To get a better overview of the project and the code there are also two diagrams
 that depict a messageflow and the module architecture and [a report detailing
 the design decisions made along the way](https://github.com/typedb-osi/typedb-client-haskell/blob/master/report.pdf).

 I will also add a browsable haddock documentation in time.
