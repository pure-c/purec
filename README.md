# Pure-C

An alternative backend for the PureScript programming language that targets C.

## Introduction

PureScript is a high-level, statically typed, functionally pure programming
language that enables various backends using its intermediate representation of
elaborated programs.

PureC provides a backend that transpiles to the C programming language, thus
enabling native, ahead-of-time compilation of PureScript programs. With a dead
simple FFI to C, performance critical sections can easily be implemented outside
of PureScript when the need arises.

For an example of how PureC could be used, take a look at
[pure-c](https://github.com/pure-c/purec-uv), a project implementing both
purescript-aff and bindings to libuv on top of PureC.

## Features

* Write programs in PureScript
* Simple FFI to C
* Generates readable, human- and machine-debuggable C
* Garbage collected (Boehm)

## Usecases

* Scrap your node.js
* Write small, easy to distribute command line utilities

## Getting Started

These instructions will get you a copy of the project up and running on your
local machine for development and testing purposes.

For now, the default makefiles use the clang compiler toolchain. So either
install clang (if not installed already), or open a PR adding support for other
compilers.

### Prerequisites

* Install node.js, including `npm`

```
make deps
```

## Build the purec utility

```
make purec
```

## Running the examples

Each example is an isolated example of how to use purec.
You can cd into each of these directories and invoke `make`.

To build all examples:

```
make examples
```

To run a particular example:

```
# ./examples/<name>/main.out
# example:
./examples/example1/main.out
```

## Running the tests

```
make test
```

## Contributing

Please read [CONTRIBUTING.md](#) for details on our code of
conduct, and the process for submitting pull requests to us.

## Versioning

This project is alpha quality and will likely remain alpha quality for a while.
That means for now there's one version, and that's `origin/HEAD`.

## Authors

* **Felix Schlitter** - *Initial work* - [felixschl](https://github.com/felixschl)

See also the list of [contributors](https://github.com/pure-c/pure-c/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details

## Acknowledgments

* [uthash](https://github.com/troydhanson/uthash) - A hash table for C structures
* [vec](https://github.com/rxi/vec) - A type-safe dynamic array implementation for C
* [ccan](https://github.com/rustyrussell/ccan) - The C Code Archive Network
* [purescript-native](https://github.com/andyarvanitis/purescript-native) - An experimental C++11/native compiler backend for PureScript
