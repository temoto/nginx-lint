What
====

nginx-lint is hlint/pylint/splint kind of tool for nginx (web server) configs.
It looks for common sources of problems in your nginx configs and outputs a list of suggestions.

http://www.nginx.org/


Usage
=====

nginxlint is written in Haskell, and so it requires GHC (or other Haskell compiler/interpreter).

Simplest way to get started is run `cabal install`. This will try to build `nginx-lint` binary using
available Haskell compiler.

Run `nginx-lint test.conf` to see examples of hint this linter may give.

For development, you may wish to run `cabal install --enable-tests`.


General usage:
    ./nginx-lint FILE...


TODO
====

Follow includes.
More hints.
