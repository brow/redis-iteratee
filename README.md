#redis-iteratee

`redis-iteratee` is one of several available Redis clients for Haskell. It differs from each other client in at least one of the following ways:

- It implements the Redis protocol in pure code, using iteratees.
- The functions it exposes are named verbatim for Redis commands: `get`, `set`, etc.
- It comes with tests.

Currently, only a small subset of commands are implemented.

#Installation

    git clone git@github.com:brow/redis-iteratee.git
    cd redis-iteratee
    cabal install

#Running tests

Two test suites are included

	git clone git@github.com:brow/redis-iteratee.git
    cabal configure --enable-tests
    cabal test
