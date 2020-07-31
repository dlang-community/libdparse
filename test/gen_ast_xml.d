/**
 * This program reads in a D source file and prints its AST in XML format to
 * stdout.
 */

import std.array;
import std.exception;
import std.file;
import std.stdio;
import dparse.ast;
import dparse.lexer;
import dparse.parser;
import dparse.astprinter;

uint errorCount;
uint warningCount;

void messageFunction(string fileName, size_t line, size_t column,
    string message, bool isError)
{
    if (isError)
    {
        errorCount++;
        stderr.writefln("%s(%d:%d)[error]: %s", fileName, line, column, message);
    }
    else
    {
        stderr.writefln("%s(%d:%d)[warn ]: %s", fileName, line, column, message);
        warningCount++;
    }
}

int main(string[] args)
{
    import dparse.rollback_allocator : RollbackAllocator;

    enforce(args.length == 2, "Must specifiy exactly one least one D file");
	auto f = File(args[1]);
	immutable ulong fileSize = f.size();
	ubyte[] fileBytes = new ubyte[](fileSize);
	enforce(f.rawRead(fileBytes).length == fileSize);
	StringCache cache = StringCache(fileSize.optimalBucketCount);
	LexerConfig config;
	config.stringBehavior = StringBehavior.source;
	config.fileName = args[1];
	const(Token)[] tokens = getTokensForParser(fileBytes, config, &cache);
	RollbackAllocator rba;
	auto mod = parseModule(ParserConfig(tokens, args[1], &rba, &messageFunction));
	auto printer = new XMLPrinter;
	printer.output = stdout;
	printer.visit(mod);
    return errorCount == 0 ? 0 : 1;
}
