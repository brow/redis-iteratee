#redis-iteratee

`redis-iteratee` is one of several Redis clients available for Haskell. It differs from each other client in at least one of the following ways:

- It implements the Redis protocol in pure code, using iteratees.
- The functions it exposes are named verbatim for Redis commands: `get`, `set`, etc.
- It comes with tests.

Currently, only a small subset of Redis commands are implemented.

#Installation

    git clone git@github.com:brow/redis-iteratee.git
    cd redis-iteratee
    cabal install

#Running tests

Two test suites are included, `test-pure` and `test-io`. The latter depends on a Redis instance running at 127.0.0.1:6379 and will fail otherwise. You can start Redis like so:
	
	redis-server

The test suite will `select` database 14 in that instance and muck around with the data in it, so make sure you're not keeping anything important there. Now do:

	git clone git@github.com:brow/redis-iteratee.git
	cd redis-iteratee
    cabal configure --enable-tests
    cabal test