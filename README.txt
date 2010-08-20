What
====

nginx-lint is hlint/pylint/splint kind of tool for nginx (web server) configs.
It looks for common sources of problems in your nginx configs and outputs a list of suggestions.

http://www.nginx.org/


Usage
=====

nginxlint is written in Haskell, and so it requires GHC (or other Haskell compiler/interpreter).

Simplest way to get started is run `make test`. This will try to build `nginxlint` binary using `ghc`
compiler and run it with example config: test.conf.


General usage:
    ./nginxlint FILE...


TODO
====

Follow includes.
More hints.
