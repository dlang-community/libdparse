/// Utility for unescaping D string literals of any kind
module dparse.strings;

import std.algorithm;
import std.array;
import std.ascii : isAlphaNum, isHexDigit, isWhite;
import std.conv;
import std.range;
import std.string;
import std.utf;

/**
 * Checks if a string literal input has correct start/end sequences (quotes) to
 * be any kind of D string literal.
 *
 * Bugs: doesn't check for validity of token strings.
 *
 * Standards: $(LINK https://dlang.org/spec/lex.html#string_literals)
 */
bool isStringLiteral(const(char)[] literal, out char stringCloseChar,
		out bool hasPostfix, out bool parseEscapes, out int prefixLength)
{
	// there are no 1 character strings
	if (literal.length < 2)
		return false;

	// check for valid start
	bool allowPostfix;
	switch (literal[0])
	{
	case 'r': // WysiwygString
	case 'x': // HexString
		if (literal[1] != '"')
			return false;
		stringCloseChar = '"';
		allowPostfix = true;
		prefixLength = 2;
		break;
	case 'q': // DelimitedString
		if (literal[1] == '{')
			stringCloseChar = '}';
		else if (literal[1] == '"')
			stringCloseChar = '"';
		else
			return false;

		allowPostfix = false;
		prefixLength = 2;
		break;
	case '`':
	case '"':
		stringCloseChar = literal[0];
		allowPostfix = true;
		parseEscapes = stringCloseChar == '"';
		prefixLength = 1;
		break;
	default:
		return false;
	}

	if (allowPostfix && literal[$ - 1].among!('c', 'w', 'd'))
	{
		hasPostfix = true;
		literal = literal[0 .. $ - 1];
	}

	if (literal.length <= prefixLength || literal[$ - 1] != stringCloseChar)
		return false;

	if (parseEscapes)
	{
		// check if end escapes the quote, making this an invalid string
		auto end = literal[0 .. $ - 1].lastIndexOfNeither("\\");
		if (end != -1)
		{
			// don't need to subtract 1
			size_t countBackslashes = literal.length - end;

			if ((countBackslashes % 2) != 0)
				return false; // uneven backslash count -> invalid end
		}
	}

	return true;
}

/// ditto
bool isStringLiteral(const(char)[] literal)
{
	char stringCloseChar;
	bool hasPostfix, parseEscapes;
	int prefixLength;
	return isStringLiteral(literal, stringCloseChar, hasPostfix, parseEscapes,
		prefixLength);
}

///
unittest
{
	assert(isStringLiteral(`"hello"`));
	assert(isStringLiteral(`"hello world!"`));
	assert(isStringLiteral(`r"hello world!"c`));
	assert(isStringLiteral(`r"hello world!"d`));
	assert(isStringLiteral(`q{cool}`));
	assert(isStringLiteral(`q{cool\}`));
	assert(isStringLiteral(`"\\"`));
	assert(!isStringLiteral(`"\\\"`));
	assert(isStringLiteral(`"\\\\"`));
	assert(isStringLiteral(`"a\\\\"`));
	assert(isStringLiteral(`""`));
	assert(isStringLiteral(`q""`));
	assert(isStringLiteral(`x""`));
	assert(!isStringLiteral(``));
	assert(!isStringLiteral(`"`));
	assert(!isStringLiteral(`w""`));
	assert(!isStringLiteral(`hello"`));
	assert(!isStringLiteral(`"hello`));
	assert(!isStringLiteral(`"hello world`));
	assert(!isStringLiteral(`hello world`));
	assert(!isStringLiteral(`r"`));
	assert(!isStringLiteral(`rr"ok"`));
	assert(!isStringLiteral(`x"`));
	assert(!isStringLiteral(`x" `));
	assert(!isStringLiteral(`qqqq`));
}

/// Defines different handler types what to do when invalid escape sequences are
/// found inside $(LREF unescapeString).
enum InvalidEscapeAction
{
	/// keep the backslash character as well as the escape characters in the
	/// string like in the input string.
	keep = 0,
	/// Ignore and skip offending characters, drop them from the output. Named
	/// character entities are still being included like $(LREF keep) as they
	/// are not currently implemented.
	skip,
	/// Throw a ConvException on invalid escape sequences. Does not throw
	/// anything on unknown named character entities as they are not currently
	/// implemented but instead treats them like $(LREF keep).
	error
}

/**
 * Unescapes a D string, effectively being the same as mixing in the string into
 * some function call, but only for single string literals.
 *
 * Strips quotes, prefixes and suffixes, interprets escape sequences in normal
 * double quoted strings and interprets hex strings. Returns simple slices for
 * non-escaped strings.
 *
 * It's undefined how invalid/malformed strings are evaluated.
 *
 * Bugs: doesn't check for validity of token strings, doesn't interpret named
 * character entity escape sequences, (HTML-kind escape sequences) doesn't check
 * nesting level of delimited strings.
 *
 * Standards: $(LINK https://dlang.org/spec/lex.html#string_literals)
 */
string unescapeString(
	InvalidEscapeAction invalidEscapeAction = InvalidEscapeAction.error
)(
	string input
)
in
{
	assert(isStringLiteral(input));
}
do
{
	char stringCloseChar;
	bool hasPostfix, parseEscapes;
	int prefixLength;
	isStringLiteral(input, stringCloseChar, hasPostfix, parseEscapes,
		prefixLength);

	if (hasPostfix)
		input = input[0 .. $ - 1];

	auto content = input[prefixLength .. $ - 1];

	if (!content.length)
		return content;

	if (input[0] == 'x')
	{
		// hex string, obsolete but still implemented
		return parseHexStringContent!invalidEscapeAction(content);
	}
	else if (input[0] == 'q' && input[1] == '"')
	{
		content = content.normalizeNewLines;
		if (isIdentifierChar(content[0]))
		{
			auto ln = content.indexOf('\n');
			if (ln == -1)
			{
				final switch (invalidEscapeAction)
				{
				case InvalidEscapeAction.keep:
					return content;
				case InvalidEscapeAction.skip:
					return null;
				case InvalidEscapeAction.error:
					throw new ConvException("Invalid delimited escape string");
				}
			}
			auto delimiter = content[0 .. ln];
			content = content[ln + 1 .. $];
			if (!content.endsWith(chain("\n", delimiter)))
			{
				final switch (invalidEscapeAction)
				{
				case InvalidEscapeAction.keep:
					return content;
				case InvalidEscapeAction.skip:
					auto lastNl = content.lastIndexOf('\n');
					if (lastNl == -1)
						return content;
					else
						return content[0 .. lastNl];
				case InvalidEscapeAction.error:
					throw new ConvException("Delimited escape string not ending correctly");
				}
			}
			return content[0 .. $ - delimiter.length];
		}
		else
		{
			char delimiterChar = content[0];
			char endChar;
			switch (delimiterChar)
			{
			case '[': endChar = ']'; break;
			case '(': endChar = ')'; break;
			case '<': endChar = '>'; break;
			case '{': endChar = '}'; break;
			default: endChar = delimiterChar; break;
			}

			if (content[1 .. $].endsWith(endChar))
				return content[1 .. $ - 1];
			else
			{
				final switch (invalidEscapeAction)
				{
				case InvalidEscapeAction.keep:
					return content;
				case InvalidEscapeAction.skip:
					return content[1 .. $];
				case InvalidEscapeAction.error:
					throw new ConvException("Invalid delimited escape string");
				}
			}
		}
	}
	else
	{
		if (!parseEscapes)
			return content.normalizeNewLines;
		else
			return unescapeDoubleQuotedContent!invalidEscapeAction(
					content.normalizeNewLines);
	}
}

///
unittest
{
	assert(unescapeString(q{r"I am Oz"}) == r"I am Oz");
	assert(unescapeString(q{r"c:\games\Sudoku.exe"}) == r"c:\games\Sudoku.exe");
	assert(unescapeString(q{r"ab\n"}) == r"ab\n");

	assert(unescapeString(q{`the Great and Powerful.`}) == `the Great and Powerful.`);
	assert(unescapeString(q{`c:\games\Empire.exe`}) == `c:\games\Empire.exe`);
	assert(unescapeString(q{`The "lazy" dog`}) == `The "lazy" dog`);
	assert(unescapeString(q{`a"b\n`}) == `a"b\n`);

	assert(unescapeString(q{"Who are you?"}) == "Who are you?");
	assert(unescapeString(q{"c:\\games\\Doom.exe"}) == "c:\\games\\Doom.exe");
	assert(unescapeString(q{"ab\n"}) == "ab\n");

	assert(unescapeString(`x"0A"`) == hexString!"0A");
	assert(unescapeString(`x"00 FBCD 32FD 0A"`) == hexString!"00 FBCD 32FD 0A");

	assert(unescapeString(`q"(foo(xxx))"`) == q"(foo(xxx))");
	assert(unescapeString(`q"[foo{]"`) == q"[foo{]");
	assert(unescapeString(`q"<foo{>"`) == q"<foo{>");
	assert(unescapeString(`q"{foo(}"`) == q"{foo(}");
	assert(unescapeString(`q"EOS
This
is a multi-line
heredoc string
EOS"`) == q"EOS
This
is a multi-line
heredoc string
EOS");
	assert(unescapeString(`q"/foo]/"`) == `foo]`);

	assert(unescapeString(`q{this is the voice of}`) == q{this is the voice of});
	assert(unescapeString(`q{/*}*/ }`) == q{/*}*/ });
	assert(unescapeString(`q{ world(q{control}); }`) == q{ world(q{control}); });
	assert(unescapeString(`q{ __TIME__ }`) == q{ __TIME__ });

	assert(unescapeString(q{"hello"c}) == "hello");
	assert(unescapeString(q{"hello"w}) == "hello");
	assert(unescapeString(q{"hello"d}) == "hello");

	assert(unescapeString(`""`) == "");
	assert(unescapeString(`"hello\'world\"cool\""`) == "hello\'world\"cool\"");
	assert(unescapeString(`"\x0A"`) == "\x0A");
	assert(unescapeString(`"\u200b"`) == "\u200b");
	assert(unescapeString(`"\U0001F4A9"`) == "\U0001F4A9");
	assert(unescapeString(`"\0"`) == "\0");
	assert(unescapeString(`"\1"`) == "\1");
	assert(unescapeString(`"\12"`) == "\12");
	assert(unescapeString(`"\127"`) == "\127");
	assert(unescapeString(`"\1278"`) == "\1278");
	assert(unescapeString(`"\12a8"`) == "\12a8");
	assert(unescapeString(`"\1a28"`) == "\1a28");
	assert(unescapeString(`x"afDE"`) == "\xaf\xDE");
	assert(unescapeString("\"hello\nworld\rfoo\r\nbar\u2028ok\u2029\"")
			== "hello\nworld\nfoo\nbar\nok\n");
}

unittest
{
	import std.exception : assertThrown;

	// unimplemented named characters
	assert(unescapeString(`"\&foo;"`) == "\\&foo;");

	assertThrown!ConvException(unescapeString(`"\&foo"`));
	assert(unescapeString!(InvalidEscapeAction.keep)(`"\&foo"`) == "\\&foo");
	assert(unescapeString!(InvalidEscapeAction.skip)(`"\&foo"`) == "");
}

unittest
{
	import std.exception : assertThrown;

	assertThrown!ConvException(unescapeString(`q"EOS"`));
	assert(unescapeString!(InvalidEscapeAction.keep)(`q"EOS"`) == "EOS");
	assert(unescapeString!(InvalidEscapeAction.skip)(`q"EOS"`) == "");

	assertThrown!ConvException(unescapeString(`q"EOS
hello"`));
	assert(unescapeString!(InvalidEscapeAction.keep)(`q"EOS
hello"`) == "hello");
	assert(unescapeString!(InvalidEscapeAction.skip)(`q"EOS
hello"`) == "hello");
	assert(unescapeString!(InvalidEscapeAction.skip)(`q"EOS
hello
world"`) == "hello");

	assertThrown!ConvException(unescapeString(`q"/xd"`));
	assert(unescapeString!(InvalidEscapeAction.keep)(`q"/xd"`) == "/xd");
	assert(unescapeString!(InvalidEscapeAction.skip)(`q"/xd"`) == "xd");

	assertThrown!ConvException(unescapeString(`"\x"`));
	assert(unescapeString!(InvalidEscapeAction.keep)(`"\x"`) == "\\x");
	assert(unescapeString!(InvalidEscapeAction.skip)(`"\x"`) == "");

	assertThrown!ConvException(unescapeString(`"\u0"`));
	assert(unescapeString!(InvalidEscapeAction.keep)(`"\u0"`) == "\\u0");
	assert(unescapeString!(InvalidEscapeAction.skip)(`"\u0"`) == "");

	assertThrown!ConvException(unescapeString(`"\U0000000"`));
	assert(unescapeString!(InvalidEscapeAction.keep)(`"\U0000000"`) == "\\U0000000");
	assert(unescapeString!(InvalidEscapeAction.skip)(`"\U0000000"`) == "");

	assertThrown!ConvException(unescapeString(`"\xAG"`));
	assert(unescapeString!(InvalidEscapeAction.keep)(`"\xAG"`) == "\\xAG");
	assert(unescapeString!(InvalidEscapeAction.skip)(`"\xAG"`) == "");

	assertThrown!ConvException(unescapeString(`"\u00AG"`));
	assert(unescapeString!(InvalidEscapeAction.keep)(`"\u00AG"`) == "\\u00AG");
	assert(unescapeString!(InvalidEscapeAction.skip)(`"\u00AG"`) == "");

	assertThrown!ConvException(unescapeDoubleQuotedContent(`a\`));
	assert(unescapeDoubleQuotedContent!(InvalidEscapeAction.keep)(`a\`) == "a\\");
	assert(unescapeDoubleQuotedContent!(InvalidEscapeAction.skip)(`a\`) == "a");

	assertThrown!ConvException(unescapeString(`"\z"`));
	assert(unescapeString!(InvalidEscapeAction.keep)(`"\z"`) == "\\z");
	assert(unescapeString!(InvalidEscapeAction.skip)(`"\z"`) == "z");

	assert(parseHexStringContent("") == "");

	assertThrown!ConvException(unescapeString(`x"AG"`));
	assert(unescapeString!(InvalidEscapeAction.keep)(`x"AG"`) == "AG");
	assert(unescapeString!(InvalidEscapeAction.skip)(`x"AG"`) == "");

	assertThrown!ConvException(unescapeString(`x"A"`));
	assert(unescapeString!(InvalidEscapeAction.keep)(`x"A"`) == "A");
	assert(unescapeString!(InvalidEscapeAction.skip)(`x"A"`) == "");
}

private string unescapeDoubleQuotedContent(
	InvalidEscapeAction invalidEscapeAction = InvalidEscapeAction.error
)(
	string input
)
{
	auto escape = input.indexOf('\\');
	if (escape == -1)
		return input;

	auto ret = appender!string;
	ret.reserve(input.length);
	size_t start = 0;

	bool requireMinLength(size_t length)
	{
		if (escape + length >= input.length)
		{
			final switch (invalidEscapeAction)
			{
			case InvalidEscapeAction.keep:
				ret ~= input[start .. $];
				start = input.length;
				return false;
			case InvalidEscapeAction.skip:
				start = input.length;
				return false;
			case InvalidEscapeAction.error:
				throw new ConvException("Unfinished escape at end of string");
			}
		}
		else
		{
			return true;
		}
	}

	void errorInvalidCharacter(size_t continueAt)
	{
		final switch (invalidEscapeAction)
		{
		case InvalidEscapeAction.keep:
			ret ~= input[start .. continueAt];
			start = continueAt;
			break;
		case InvalidEscapeAction.skip:
			start = continueAt;
			break;
		case InvalidEscapeAction.error:
			throw new ConvException("Invalid escape character before index "
					~ continueAt.to!string);
		}
	}

	bool parseUnicode(size_t length)
	{
		auto c = input[escape + 2 .. escape + 2 + length];
		if (!c.all!isHexDigit)
		{
			errorInvalidCharacter(escape + 2 + length);
			return false;
		}
		dchar ch = cast(dchar) c.to!uint(16);
		char[4] buf;
		auto size = encode(buf, ch);
		ret ~= buf[0 .. size];
		start = escape + 2 + length;
		return true;
	}

	Loop: while (escape != -1)
	{
		ret ~= input[start .. escape];
		start = escape;

		if (!requireMinLength(1))
			break;

	Switch:
		switch (input[escape + 1])
		{
		case '\'':
		case '"':
		case '?':
		case '\\':
			ret ~= input[escape + 1];
			start = escape + 2;
			break;

		case 'a': ret ~= '\a'; start = escape + 2; break;
		case 'b': ret ~= '\b'; start = escape + 2; break;
		case 'f': ret ~= '\f'; start = escape + 2; break;
		case 'n': ret ~= '\n'; start = escape + 2; break;
		case 'r': ret ~= '\r'; start = escape + 2; break;
		case 't': ret ~= '\t'; start = escape + 2; break;
		case 'v': ret ~= '\v'; start = escape + 2; break;

		case 'x':
			if (!requireMinLength(3))
				break Loop;
			char a = input[escape + 2];
			char b = input[escape + 3];
			if (!a.isHexDigit || !b.isHexDigit)
			{
				errorInvalidCharacter(escape + 4);
				break;
			}
			ret ~= cast(char)(a.parseHexChar << 4 | b.parseHexChar);
			start = escape + 4;
			break;
		case 'u':
			if (!requireMinLength(1 + 4))
				break Loop;
			parseUnicode(4);
			break;
		case 'U':
			if (!requireMinLength(1 + 8))
				break Loop;
			parseUnicode(8);
			break;
		case '0': .. case '7':
			int length = 1;
			foreach (n; 2 .. 4)
			{
				if (escape + 1 + n > input.length)
					break;
				char c = input[escape + n];
				if (c >= '0' && c <= '7')
					length = n;
				else
					break;
			}
			int c = input[escape + 1 .. escape + 1 + length].to!int(8);
			ret ~= cast(char) c;
			start = escape + 1 + length;
			break;
		case '&':
			auto end = input.indexOf(';', escape + 2);
			if (end == -1)
			{
				errorInvalidCharacter(input.length);
			}
			else
			{
				ret ~= input[escape .. end + 1];
				start = end + 1;
			}
			break;
		default:
			errorInvalidCharacter(escape + 1);
			break;
		}

		escape = input.indexOf('\\', start);
	}
	ret ~= input[start .. $];
	return ret.data;
}

unittest
{
	assert(unescapeDoubleQuotedContent(`hello world`) == "hello world");
	assert(unescapeDoubleQuotedContent(`hello\nworld`) == "hello\nworld");
	assert(unescapeDoubleQuotedContent(`hello\tworld`) == "hello\tworld");
	assert(unescapeDoubleQuotedContent(`hello\u200bworld`) == "hello\u200bworld");
	assert(unescapeDoubleQuotedContent(`hello \"\\ok`) == "hello \"\\ok");
}

private string parseHexStringContent(
	InvalidEscapeAction invalidEscapeAction = InvalidEscapeAction.error
)(
	string input
)
{
	if (!input.length)
		return input;

	auto ret = appender!string;
	ret.reserve(input.length / 3);
	char buf;
	foreach (i, char c; input)
	{
		if (c.isWhite)
			continue;

		if (!c.isHexDigit)
		{
			final switch (invalidEscapeAction)
			{
			case InvalidEscapeAction.keep:
				if (buf != char.init)
				{
					ret ~= buf;
					buf = char.init;
				}
				ret ~= c;
				break;
			case InvalidEscapeAction.skip:
				break;
			case InvalidEscapeAction.error:
				throw new ConvException("Invalid hex character at index "
						~ i.to!string);
			}
		}
		else
		{
			if (buf == char.init)
			{
				buf = c;
			}
			else
			{
				ret ~= cast(char)(buf.parseHexChar << 4 | c.parseHexChar);
				buf = char.init;
			}
		}
	}

	if (buf != char.init)
	{
		final switch (invalidEscapeAction)
		{
		case InvalidEscapeAction.keep:
			ret ~= buf;
			break;
		case InvalidEscapeAction.skip:
			break;
		case InvalidEscapeAction.error:
			throw new ConvException("Unterminated hex character at end of string");
		}
	}

	return ret.data;
}

private int parseHexChar(char c)
in
{
	assert(c.isHexDigit);
	assert('a' > 'A' && 'A' > '0'); // just checking that ASCII doesn't suddenly change
}
do
{
	// can omit range ends and digit check because of function preconditions
	if (c >= 'a')
		return (c - 'a') + 10;
	else if (c >= 'A')
		return (c - 'A') + 10;
	else
		return c - '0';
}

private bool isIdentifierChar(char c)
{
	return isAlphaNum(c) || c == '_';
}

/// normalizes all line endings with \n, as parsed in D strings
private string normalizeNewLines(string text)
{
	import std.utf : codeLength;

	enum exoticLineBreakLength = codeLength!char('\u2028');
	static immutable dchar[] nlCharacters = ['\r', '\u2028', '\u2029'];

	auto end = text.indexOfAny(nlCharacters);
	if (end == -1)
		return text;
	auto ret = appender!string;
	ret.reserve(text.length);
	size_t start = 0;
	while (end != -1)
	{
		ret ~= text[start .. end];
		ret ~= '\n';
		if (end + 1 < text.length && text[end] == '\r' && text[end + 1] == '\n')
			end++;
		else if (text[end] != '\r')
			end += exoticLineBreakLength - 1;
		start = end + 1;
		end = text.indexOfAny(nlCharacters, start);
	}
	ret ~= text[start .. $];
	return ret.data;
}

///
unittest
{
	string testNoChange = "hello\nworld!";
	assert(normalizeNewLines(testNoChange).ptr is testNoChange.ptr);

	assert(normalizeNewLines("hello\rworld") == "hello\nworld");
	assert(normalizeNewLines("hello\r\nworld") == "hello\nworld");
	assert(normalizeNewLines("hello\r\n\nworld") == "hello\n\nworld");
	assert(normalizeNewLines("hello\u2028\nworld") == "hello\n\nworld");
	assert(normalizeNewLines("hello\u2029\nworld") == "hello\n\nworld");
	assert(normalizeNewLines("hello\r") == "hello\n");
}
