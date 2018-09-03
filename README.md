# Pure-C

An alternative backend for the PureScript programming language that targets C
using Clang.

## Getting Started

These instructions will get you a copy of the project up and running on your
local machine for development and testing purposes.

### Prerequisites

* Install node.js, including `npm`
* Install global command line utilities for working with PureScript in general

```
npm i -g \
	bower \
	purescript \
	pulp
```

* Install project dependencies

```
bower install
```

##### Ubuntu 16.04

```
apt-get install \
	libblocksruntime-dev \
	uthash-dev \
	uthash-dev \
	libgc-dev
```

## Running the tests

**NOTE** The test suite is still being built out [#12](https://github.com/pure-c/pure-c/issues/12).
For now this has to suffice:


```
pulp test && make example1
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
