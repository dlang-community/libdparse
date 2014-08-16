import std.array;
import std.exception;
import std.file;
import std.stdio;
import std.d.ast;
import std.d.lexer;
import std.d.parser;

int errorCount = 0;
int warningCount = 0;

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
		stdout.writefln("%s(%d:%d)[warn ]: %s", fileName, line, column, message);
		warningCount++;
	}
}

int main(string[] args)
{
	enforce(args.length > 1, "Must specifiy at least one D file");
	foreach (arg; args[1 .. $])
	{
		auto f = File(arg);
		immutable ulong fileSize = f.size();
		ubyte[] fileBytes = new ubyte[](fileSize);
		enforce(f.rawRead(fileBytes).length == fileSize);
		StringCache cache = StringCache(StringCache.defaultBucketCount);
		LexerConfig config;
		config.stringBehavior = StringBehavior.source;
		config.fileName = arg;
		const(Token)[] tokens = getTokensForParser(fileBytes, config, &cache);
		parseModule(tokens, arg, null, &messageFunction);
	}
	writefln("Finished parsing with %d errors and %d warnings.",
			errorCount, warningCount);
	return errorCount == 0 ? 0 : 1;
}
