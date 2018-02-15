libdparse
=========
Library for lexing and parsing D source code.

# Documentation

Online documentation is available [here](http://libdparse.dlang.io).

A HTML version of libdparse's grammar is also [automatically generated](http://libdparse.dlang.io/grammar.html).

# Testing
[![CI Status](https://travis-ci.org/dlang-community/libdparse.svg)](https://travis-ci.org/dlang-community/libdparse)

Tests are present in the test directory. To run them execute the run\_tests.sh
script. Running the tests on Windows is not currently supported.

# Differences with the official grammar
* [Static array initialization syntax](http://dlang.org/arrays.html#static-init-static). Due to ambiguities they are supported when the expression that gives the elements indexes is not an array. In the opposite case they are parsed as associative array literals.

# Unsupported Syntax
* [Class allocators](http://dlang.org/class.html#allocators). These are deprecated in D2.
* [Class deallocators](http://dlang.org/class.html#deallocators). These are deprecated in D2.

# Example

```d
/+dub.sdl:
dependency "libdparse" version="~>0.7"
+/
import dparse.ast;
import std.stdio, std.range;

class TestVisitor : ASTVisitor
{
    alias visit = ASTVisitor.visit;
    int indentLevel;

    override void visit(const FunctionDeclaration decl)
    {
        writeln(' '.repeat(indentLevel * 4), decl.name.text);
        indentLevel++;
        scope (exit) indentLevel--;
        decl.accept(this);
    }
}

void main()
{
    import dparse.lexer;
    import dparse.parser : parseModule;
    import dparse.rollback_allocator : RollbackAllocator;

    auto sourceCode = q{
        void foo() @safe {
            void bar();
        }
    };
    LexerConfig config;
    auto cache = StringCache(StringCache.defaultBucketCount);
    auto tokens = getTokensForParser(sourceCode, config, &cache);

    RollbackAllocator rba;
    auto m = parseModule(tokens, "test.d", &rba);
    auto visitor = new TestVisitor();
    visitor.visit(m);
}
```
[![Open on run.dlang.io](https://img.shields.io/badge/run.dlang.io-open-blue.svg)](https://run.dlang.io/is/qZsGDD)
