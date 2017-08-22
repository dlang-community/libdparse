libdparse
=========
Library for lexing and parsing D source code.

# Documentation
Online documentation is available [here](https://dlang-community.github.io/libdparse)

# Testing
[![CI Status](https://travis-ci.org/dlang-community/libdparse.svg)](https://travis-ci.org/dlang-community/libdparse)

Tests are present in the test directory. To run them execute the run\_tests.sh
script. Running the tests on Windows is not currently supported.

# Unsupported Syntax
* [Static array initialization syntax](http://dlang.org/arrays.html#static-init-static). Due to ambiguities they are supported when the expression that gives the elements indexes is not an array. In the opposite case they parsed as associative array literals.
* [Class allocators](http://dlang.org/class.html#allocators). These are deprecated in D2.
* [Class deallocators](http://dlang.org/class.html#deallocators). These are deprecated in D2.
