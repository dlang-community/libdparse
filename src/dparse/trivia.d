/**
 * Module to work with trivia tokens (`comment`, `whitespace`,
 * `specialTokenSequence`) which are attached to tokens near them when source
 * code gets tokenized.
 */
module dparse.trivia;

import std.algorithm;
import std.array;
import std.range;
import std.string;
import std.traits;

import dparse.lexer;

enum CommentType : ubyte
{
    none,
    docLine,
    docBlock,
    normalLine,
    normalBlock,
}

CommentType determineCommentType(string comment) pure nothrow @safe
{
    auto bytes = comment.representation;
    auto index = bytes.startsWith(
        "//".representation,
        "/+".representation,
        "/*".representation
    );
    bool isDoc = bytes.length >= 3 && bytes[1] == bytes[2];
    switch (index)
    {
    case 1:
        // Don't treat "////////...." comments as doc comments
        isDoc = isDoc && (bytes.length == 3 || bytes[3..$].any!(c => c != '/'));
        return isDoc ? CommentType.docLine :  CommentType.normalLine;
    case 2:
    case 3:
        return isDoc ? CommentType.docBlock :  CommentType.normalBlock;
    default:
        return CommentType.none;
    }
}

///
unittest
{
    assert (determineCommentType("/// hello") == CommentType.docLine);
    assert (determineCommentType("/++ hello") == CommentType.docBlock);
    assert (determineCommentType("/** hello") == CommentType.docBlock);
    assert (determineCommentType("// hello") == CommentType.normalLine);
    assert (determineCommentType("/+ hello") == CommentType.normalBlock);
    assert (determineCommentType("/* hello") == CommentType.normalBlock);
    assert (determineCommentType("/ hello") == CommentType.none);
    assert (determineCommentType("/") == CommentType.none);

    assert (determineCommentType("////////////////////") == CommentType.normalLine);
    assert (determineCommentType("///") == CommentType.docLine);
    assert (determineCommentType("///   ") == CommentType.docLine);
}

bool isDocComment(CommentType type) @safe nothrow pure
{
    return type == CommentType.docLine || type == CommentType.docBlock;
}

/**
 * Removes "decoration" such as leading whitespace, leading + and * characters,
 * and places the result into the given output range
 */
public void unDecorateComment(T)(string comment, auto ref T outputRange)
if (isOutputRange!(T, string))
in
{
    assert (comment.length >= 3);
}
do
{
    import std.string : chompPrefix, KeepTerminator, lineSplitter, stripRight;

    string leadingChars;

    enum LineType { none, normal, strange }
    LineType prevLineType;

    switch (comment[0 .. 3])
    {
    case "///":
        foreach (line; lineSplitter!(KeepTerminator.yes)(comment))
        {
            if (leadingChars.empty)
            {
                size_t k = 3;
                while (k < line.length && (line[k] == ' ' || line[k] == '\t'))
                    k++;
                leadingChars = line[0 .. k];
            }
            outputRange.put(line.chompPrefix(leadingChars));
        }
        break;
    case "/++":
    case "/**":
        alias CL = MultiLineCommentHelper!(ElementEncodingType!(typeof(comment)));
        CL cl = CL(comment);
        cl.process(outputRange);
        break;
    default:
        outputRange.put(comment);
    }
}

///
unittest
{
    import std.array:array, appender;
    import std.stdio:stderr;
    stderr.writeln("Running unittest for unDecorateComment...");

    string[] inputs = [
        "/***************\n*******************/",
        "/***************\n *\n ******************/",
        "/**\n*/",
        "/** */",
        "/***/",
        "/******/",
        "/** abcde1 */",
        "/// abcde2\n/// abcde2",
        "/**\n * stuff1\n */",
        "/**\n *\n * stuff2\n */",
        "/**\n *\n * stuff3\n *\n */",
        "/**\n *\n * stuff4\n *\n*/",
        "/**\n *  abcde3\n *    abcde3 \n */",
        "/**\n * abcde4\n *\n * abcde4\n */",
        "/**abcde5\n*abcde5\n*/",
        "/** abcde6\n * abcde6\n*/",
        "/**\n1\n\n\n\n*/",
        "/**\r\n1\r\n\r\n\r\n\r\n*/",
        "/**\na1\n\na2\n\n*/",
        "/**b1\n*b2\n*b3*/",
        "/**c1\n    *c2\n    *c3*/",
        "/**d1\n    *d2\n    *d3\n*/",
        "///a\fbc\n///def"
    ];
    string[] outputs = [
        "",
        "",
        "",
        "",
        "",
        "",
        "abcde1",
        "abcde2\nabcde2",
        "stuff1",
        "stuff2",
        "stuff3",
        "stuff4",
        "abcde3\n  abcde3",
        "abcde4\n\nabcde4",
        "abcde5\nabcde5",
        "abcde6\nabcde6",
        "1",
        "1",
        "a1\n\na2",
        "b1\nb2\nb3",
        "c1\nc2\nc3",
        "d1\nd2\nd3",
        "a\fbc\ndef"
    ];

    // tests where * and + are not interchangeable
    string[2][] np =
    [
        ["/**\n * d1\n d2\n */", "* d1\nd2"],
        ["/**\n + d1\n d2\n */", "+ d1\nd2"],
        ["/**d1\n\n\n*d2\n*/",  "d1\n\n*d2"],
    ];

    assert(inputs.length == outputs.length);
    foreach (pair; zip(inputs, outputs))
    {
        foreach (b; [true, false])
        {
            auto app = appender!string();
            unDecorateComment(b ? pair[0] : pair[0].replace("*", "+"), app);
            assert(pair[1] == app.data, "[[" ~ pair[0] ~ "]] => [[" ~ app.data ~ "]]");
        }
    }
    foreach (pair; np)
    {
        auto app = appender!string();
        unDecorateComment(pair[0], app);
        assert(pair[1] == app.data, "[[" ~ pair[0] ~ "]] => [[" ~ app.data ~ "]]");
    }
    stderr.writeln("Unittest for unDecorateComment passed.");
}

/** Gives a line per line view on DDOC comments of type `/++` and `/**` which
 * makes easier to remove the decoration and in an almost 100% nogc way. */
private struct MultiLineCommentHelper(CharType : const(char))
{
    // this struct is more used as a 'function with nested functions' would.
    this() @disable;
    this(this) @disable;
    auto opAssign(T)(T t) @disable;

private:

    char[][] lines;
    // either lines.length or lines.length-1, depending on if last line only closes
    size_t lastLineInBlockPlusOne;
    // either '*' or '+'
    const(char) commentChar;
    // either 0 or 1, depending on if first line only opens
    ubyte firstLineInBlock;

    import std.ascii : isWhite;

    void stripIndent() @safe @nogc pure nothrow
    {
        if (lines.length < 2)
            return;
        size_t count;
        foreach (const j; 0 .. lines[1].length)
            if (!(lines[1][j]).isWhite)
        {
            count = j;
            break;
        }
        if (count < 2)
            return;
        foreach (ref line; lines[1 .. $])
        {
            foreach (const j; 0 .. line.length)
            {
                if (!(line[j]).isWhite)
                    break;
                if (j == count - 1)
                {
                    line = line[j .. $];
                    break;
                }
            }
        }
    }

    void processFirstLine() @safe @nogc pure nothrow
    {
        assert(lines.length);
        if (lines[0].length > 3)
        {
            foreach (const i; 1..lines[0].length)
            {
                if (lines[0][i] == commentChar)
                {
                    if (i < lines[0].length - 2)
                        continue;
                    if (i == lines[0].length - 2 && lines[0][i+1] == '/')
                    {
                        lines[0][] = ' ';
                        break;
                    }
                    if (i == lines[0].length - 1)
                    {
                        lines[0][] = ' ';
                        break;
                    }
                }
                else
                {
                    lines[0][0..i] = ' ';
                    break;
                }
            }
        }
        lines[0][0..3] = "   ";
        if (lines.length == 1 &&
            lines[0][$-2] == commentChar && lines[0][$-1] == '/')
        {
            lines[0][$-2..$] = "  ";
        }
        foreach (const i; 0..lines[0].length)
            if (!(lines[0][i].isWhite))
                return;
        firstLineInBlock = 1;
    }

    void processLastLine() @safe @nogc pure nothrow
    {
        lastLineInBlockPlusOne = lines.length;
        if (lines.length == 1)
            return;
        size_t closeStartIndex = size_t.max;
        foreach (const i; 0..lines[$-1].length)
        {
            if (lines[$-1][i] == commentChar)
            {
                if (closeStartIndex == size_t.max)
                    closeStartIndex = i;
                if (i == lines[$-1].length - 2)
                {
                    // see the FIXME note in unDecorate()
                    lastLineInBlockPlusOne = closeStartIndex == 0 ? lines.length-1 : lines.length;

                    lines[$-1][closeStartIndex..$] = ' ';
                    break;
                }
            }
            else
            {
                closeStartIndex = size_t.max;
                lastLineInBlockPlusOne = lines.length;
            }
        }
    }

    void unDecorate() @safe @nogc pure nothrow
    {
        if (lines.length == 1 || lines.length == 2 && lines[$-1].length == 0)
            return;
        bool allDecorated;
        static immutable char[2][2] pattern = [[' ', '*'],[' ', '+']];
        const ubyte patternIndex = commentChar == '+';
        // first line is never decorated
        const size_t lo = 1;
        // although very uncommon, the last line can be decorated e.g in `* lastline */`:
        // the first '*' is a deco if all prev lines are also decorated.
        // FIXME: `hi` should be set to `lastLineInBlockPlusOne`...
        const size_t hi = (lines[$-1].length > 1 &&
            (lines[$-1][0] == commentChar || lines[$-1][0..2] == pattern[patternIndex]))
            ?  lines.length : lines.length-1;
        // deco with a leading white
        foreach (const i; lo .. hi)
        {
            if (lines[i].length < 2)
                break;
            else if (lines[i][0..2] != pattern[patternIndex])
                break;
            else if (i == hi-1)
                allDecorated = true;
        }
        // deco w/o leading white
        if (!allDecorated)
            foreach (const i; lo .. hi)
        {
            if (lines[i].length == 0)
                break;
            if (lines[i][0] != commentChar)
                break;
            else if (i == hi-1)
                allDecorated = true;
        }
        if (!allDecorated)
            return;

        const size_t indexToChange = (lines[lo][0] == commentChar) ? 0 : 1;
        foreach (ref line; lines[lo .. hi])
            line[indexToChange] = ' ';
    }

    void stripLeft() @safe @nogc pure nothrow
    {
        foreach (const i; 0 .. lines[0].length)
            if (!(lines[0][i]).isWhite)
        {
            lines[0] = lines[0][i..$];
            break;
        }
        if (lines.length == 1)
            return;
        while (true)
        {
            bool processColumn;
            foreach (ref line; lines[1 .. lastLineInBlockPlusOne])
            {
                if (line.length == 0)
                    continue;
                if (!(line[0]).isWhite)
                    return;
                processColumn = true;
            }
            if (!processColumn)
                return;
            foreach (ref line; lines[1 .. lastLineInBlockPlusOne])
            {
                if (line.length == 0)
                    continue;
                line = line[1..$];
            }
        }
    }

    void stripRight() @safe @nogc pure nothrow
    {
        foreach (ref line; lines[0 .. lines.length])
        {
            if (line.length == 0)
                continue;
            if ((line[$-1]).isWhite)
            {
                size_t firstWhite = line.length;
                while (firstWhite > 0 && (line[firstWhite-1]).isWhite)
                    firstWhite--;
                line = line[0..firstWhite];
            }
        }
    }

    void run() @safe @nogc pure nothrow
    {
        stripIndent();
        processFirstLine();
        processLastLine();
        unDecorate();
        stripLeft();
        stripRight();
    }

public:

    this(CharType[] text) @safe pure nothrow
    {
        assert(text.length >= 3 && text[0] == '/',
            "MultiLineCommentHelper text must start with a comment in form /++ or /**");

        commentChar = text[1];
        size_t startIndex, i;
        Appender!(char[][]) linesApp;
        linesApp.reserve(512);

        void storeLine(size_t endIndexPlusOne)
        {
            static if (isMutable!CharType)
                linesApp ~= text[startIndex..endIndexPlusOne];
            else
                linesApp ~= text[startIndex..endIndexPlusOne].dup;
        }

        // if we go over text length (in \r\n) we already stored the line, so just exit there
        while (i < text.length)
        {
            // check if next char is going to be end of text, store until then & break
            if (i + 1 == text.length)
            {
                storeLine(text.length);
                break;
            }
            if (text[i] == '\n')
            {
                storeLine(i);
                startIndex = i + 1;
            }
            else if (i + 1 < text.length && text[i .. i+2] == "\r\n")
            {
                storeLine(i);
                i++;
                startIndex = i + 1;
            }
            i++;
        }
        lines = linesApp.data;
    }

    void process(T)(ref T outbuffer)
    {
        run();
        outbuffer.reserve(lines.length * 90);
        bool prevWritten, empties;
        foreach (ref line; lines[firstLineInBlock .. lines.length])
        {
            if (line.length != 0)
            {
                // close preceeding line
                if (prevWritten)
                    outbuffer ~= "\n";
                // insert new empty line
                if (prevWritten && empties)
                    outbuffer ~= "\n";

                outbuffer ~= line;
                prevWritten = true;
                empties = false;
            }
            else empties = true;
        }
    }
}

unittest
{
    import std.conv : to;

    alias SC = MultiLineCommentHelper!(immutable(char));

    // checks full comment processing on the given string and compares the generated lines
    void check(string comment, string[] lines, size_t lineNo = __LINE__)
    {
        auto sc = SC(comment);
        sc.run();
        assert(sc.lines == lines, sc.lines.to!string ~ " != " ~ lines.to!string
            ~ " (for check on line " ~ lineNo.to!string ~ ")");
    }

    // check common cases while typing
    check("/++", [""]);
    check("/++\r", [""]);
    check("/++\n", [""]);
    check("/++\r\n", [""]);
    check("/++\r\n+", ["", "+"]);
    check("/++\r\n+ ok", ["", "ok"]);
    check("/++\r\n+ ok\r\n+/", ["", "ok", ""]);
    check("/++/", [""]);
}

/// Extracts and combines ddoc comments from trivia comments.
string extractDdocFromTrivia(Tokens)(Tokens tokens) pure nothrow @safe
    if (isInputRange!Tokens && is(ElementType!Tokens : Token))
{
    bool hasDoc;
    auto ret = appender!string;
    foreach (trivia; tokens)
    {
        if (trivia.type == tok!"comment"
            && trivia.text.determineCommentType.isDocComment)
        {
            hasDoc = true;
            if (!ret.data.empty)
                ret.put('\n');
            unDecorateComment(trivia.text, ret);
        }
    }

    if (ret.data.length)
        return ret.data;
    else
        return hasDoc ? "" : null;
}

unittest
{
    Token[] tokens = [
        Token(cast(ubyte) tok!"whitespace", "\n\n", 0, 0, 0),
        Token(cast(ubyte) tok!"comment", "///", 0, 0, 0),
        Token(cast(ubyte) tok!"whitespace", "\n", 0, 0, 0)
    ];

    // Empty comment is non-null
    auto comment = extractDdocFromTrivia(tokens);
    assert(comment !is null);
    assert(comment == "");

    // Missing comment is null
    comment = extractDdocFromTrivia(tokens[0 .. 1]);
    assert(comment is null);
    assert(comment == "");
}

string extractLeadingDdoc(const Token token) pure nothrow @safe
{
    return extractDdocFromTrivia(token.leadingTrivia);
}

string extractTrailingDdoc(const Token token) pure nothrow @safe
{
    return extractDdocFromTrivia(token.trailingTrivia.filter!(a => a.line == token.line));
}

// test token trivia members
unittest
{
    import std.conv : to;
    import std.exception : enforce;

    static immutable src = `/// this is a module.
// mixed
/// it can do stuff
module foo.bar;

// hello

/**
 * some doc
 * hello
 */
int x; /// very nice

// TODO: do stuff
void main() {
    #line 40
    /// could be better
    writeln(":)");
}

///
unittest {}

/// end of file`;

    LexerConfig cf;
    StringCache ca = StringCache(16);

    const tokens = getTokensForParser(src, cf, &ca);

    assert(tokens.length == 22);

    assert(tokens[0].type == tok!"module");
    assert(tokens[0].leadingTrivia.length == 6);
    assert(tokens[0].leadingTrivia[0].type == tok!"comment");
    assert(tokens[0].leadingTrivia[0].text == "/// this is a module.");
    assert(tokens[0].leadingTrivia[1].type == tok!"whitespace");
    assert(tokens[0].leadingTrivia[1].text == "\n");
    assert(tokens[0].leadingTrivia[2].type == tok!"comment");
    assert(tokens[0].leadingTrivia[2].text == "// mixed");
    assert(tokens[0].leadingTrivia[3].type == tok!"whitespace");
    assert(tokens[0].leadingTrivia[3].text == "\n");
    assert(tokens[0].leadingTrivia[4].type == tok!"comment");
    assert(tokens[0].leadingTrivia[4].text == "/// it can do stuff");
    assert(tokens[0].leadingTrivia[5].type == tok!"whitespace");
    assert(tokens[0].leadingTrivia[5].text == "\n");
    assert(tokens[0].trailingTrivia.length == 1);
    assert(tokens[0].trailingTrivia[0].type == tok!"whitespace");
    assert(tokens[0].trailingTrivia[0].text == " ");

    assert(tokens[1].type == tok!"identifier");
    assert(tokens[1].text == "foo");
    assert(!tokens[1].leadingTrivia.length);
    assert(!tokens[1].trailingTrivia.length);

    assert(tokens[2].type == tok!".");
    assert(!tokens[2].leadingTrivia.length);
    assert(!tokens[2].trailingTrivia.length);

    assert(tokens[3].type == tok!"identifier");
    assert(tokens[3].text == "bar");
    assert(!tokens[3].leadingTrivia.length);
    assert(!tokens[3].trailingTrivia.length);

    assert(tokens[4].type == tok!";");
    assert(!tokens[4].leadingTrivia.length);
    assert(tokens[4].trailingTrivia.length == 1);
    assert(tokens[4].trailingTrivia[0].type == tok!"whitespace");
    assert(tokens[4].trailingTrivia[0].text == "\n\n");

    assert(tokens[5].type == tok!"int");
    assert(tokens[5].leadingTrivia.length == 4);
    assert(tokens[5].leadingTrivia[0].text == "// hello");
    assert(tokens[5].leadingTrivia[1].text == "\n\n");
    assert(tokens[5].leadingTrivia[2].text == "/**\n * some doc\n * hello\n */");
    assert(tokens[5].leadingTrivia[3].text == "\n");
    assert(tokens[5].trailingTrivia.length == 1);
    assert(tokens[5].trailingTrivia[0].text == " ");

    assert(tokens[6].type == tok!"identifier");
    assert(tokens[6].text == "x");
    assert(!tokens[6].leadingTrivia.length);
    assert(!tokens[6].trailingTrivia.length);

    assert(tokens[7].type == tok!";");
    assert(!tokens[7].leadingTrivia.length);
    assert(tokens[7].trailingTrivia.length == 3);
    assert(tokens[7].trailingTrivia[0].text == " ");
    assert(tokens[7].trailingTrivia[1].text == "/// very nice");
    assert(tokens[7].trailingTrivia[2].text == "\n\n");

    assert(tokens[8].type == tok!"void");
    assert(tokens[8].leadingTrivia.length == 2);
    assert(tokens[8].leadingTrivia[0].text == "// TODO: do stuff");
    assert(tokens[8].leadingTrivia[1].text == "\n");
    assert(tokens[8].trailingTrivia.length == 1);
    assert(tokens[8].trailingTrivia[0].text == " ");

    assert(tokens[9].type == tok!"identifier");
    assert(tokens[9].text == "main");
    assert(!tokens[9].leadingTrivia.length);
    assert(!tokens[9].trailingTrivia.length);

    assert(tokens[10].type == tok!"(");
    assert(!tokens[10].leadingTrivia.length);
    assert(!tokens[10].trailingTrivia.length);

    assert(tokens[11].type == tok!")");
    assert(!tokens[11].leadingTrivia.length);
    assert(tokens[11].trailingTrivia.length == 1);
    assert(tokens[11].trailingTrivia[0].text == " ");

    assert(tokens[12].type == tok!"{");
    assert(!tokens[12].leadingTrivia.length);
    assert(tokens[12].trailingTrivia.length == 1);
    assert(tokens[12].trailingTrivia[0].text == "\n    ");

    assert(tokens[13].type == tok!"identifier");
    assert(tokens[13].text == "writeln");
    assert(tokens[13].leadingTrivia.length == 4);
    assert(tokens[13].leadingTrivia[0].type == tok!"specialTokenSequence");
    assert(tokens[13].leadingTrivia[0].text == "#line 40");
    assert(tokens[13].leadingTrivia[1].type == tok!"whitespace");
    assert(tokens[13].leadingTrivia[1].text == "\n    ");
    assert(tokens[13].leadingTrivia[2].type == tok!"comment");
    assert(tokens[13].leadingTrivia[2].text == "/// could be better");
    assert(tokens[13].leadingTrivia[3].type == tok!"whitespace");
    assert(tokens[13].leadingTrivia[3].text == "\n    ");
    assert(!tokens[13].trailingTrivia.length);

    assert(tokens[14].type == tok!"(");
    assert(!tokens[14].leadingTrivia.length);
    assert(!tokens[14].trailingTrivia.length);

    assert(tokens[15].type == tok!"stringLiteral");
    assert(!tokens[15].leadingTrivia.length);
    assert(!tokens[15].trailingTrivia.length);

    assert(tokens[16].type == tok!")");
    assert(!tokens[16].leadingTrivia.length);
    assert(!tokens[16].trailingTrivia.length);

    assert(tokens[17].type == tok!";");
    assert(!tokens[17].leadingTrivia.length);
    assert(tokens[17].trailingTrivia.length == 1);
    assert(tokens[17].trailingTrivia[0].text == "\n");

    assert(tokens[18].type == tok!"}");
    assert(!tokens[18].leadingTrivia.length);
    assert(tokens[18].trailingTrivia.length == 1);
    assert(tokens[18].trailingTrivia[0].type == tok!"whitespace");
    assert(tokens[18].trailingTrivia[0].text == "\n\n");

    assert(tokens[19].type == tok!"unittest");
    assert(tokens[19].leadingTrivia.length == 2);
    assert(tokens[19].leadingTrivia[0].type == tok!"comment");
    assert(tokens[19].leadingTrivia[0].text == "///");
    assert(tokens[19].leadingTrivia[1].type == tok!"whitespace");
    assert(tokens[19].leadingTrivia[1].text == "\n");

    assert(tokens[19].trailingTrivia.length == 1);
    assert(tokens[19].trailingTrivia[0].type == tok!"whitespace");
    assert(tokens[19].trailingTrivia[0].text == " ");

    assert(tokens[20].type == tok!"{");
    assert(!tokens[20].leadingTrivia.length);
    assert(!tokens[20].trailingTrivia.length);

    assert(tokens[21].type == tok!"}");
    assert(!tokens[21].leadingTrivia.length);
    assert(tokens[21].trailingTrivia.length == 2);
    assert(tokens[21].trailingTrivia[0].type == tok!"whitespace");
    assert(tokens[21].trailingTrivia[0].text == "\n\n");
    assert(tokens[21].trailingTrivia[1].type == tok!"comment");
    assert(tokens[21].trailingTrivia[1].text == "/// end of file");
}
