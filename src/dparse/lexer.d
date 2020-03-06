module dparse.lexer;

import std.typecons;
import std.typetuple;
import std.array;
import std.algorithm;
import std.range;
import std.experimental.lexer;
import std.traits;
import core.cpuid : sse42;
version (D_InlineAsm_X86_64)
{
    version (Windows) {}
    else version = iasm64NotWindows;
}

/// Operators
private enum operators = [
    ",", ".", "..", "...", "/", "/=", "!", "!<", "!<=", "!<>", "!<>=", "!=",
    "!>", "!>=", "$", "%", "%=", "&", "&&", "&=", "(", ")", "*", "*=", "+", "++",
    "+=", "-", "--", "-=", ":", ";", "<", "<<", "<<=", "<=", "<>", "<>=", "=",
    "==", "=>", ">", ">=", ">>", ">>=", ">>>", ">>>=", "?", "@", "[", "]", "^",
    "^=", "^^", "^^=", "{", "|", "|=", "||", "}", "~", "~="
];

/// Kewords
private enum keywords = [
    "abstract", "alias", "align", "asm", "assert", "auto", "bool",
    "break", "byte", "case", "cast", "catch", "cdouble", "cent", "cfloat",
    "char", "class", "const", "continue", "creal", "dchar", "debug", "default",
    "delegate", "delete", "deprecated", "do", "double", "else", "enum",
    "export", "extern", "false", "final", "finally", "float", "for", "foreach",
    "foreach_reverse", "function", "goto", "idouble", "if", "ifloat",
    "immutable", "import", "in", "inout", "int", "interface", "invariant",
    "ireal", "is", "lazy", "long", "macro", "mixin", "module", "new", "nothrow",
    "null", "out", "override", "package", "pragma", "private", "protected",
    "public", "pure", "real", "ref", "return", "scope", "shared", "short",
    "static", "struct", "super", "switch", "synchronized", "template", "this",
    "throw", "true", "try", "typedef", "typeid", "typeof", "ubyte", "ucent",
    "uint", "ulong", "union", "unittest", "ushort", "version", "void",
    "wchar", "while", "with", "__DATE__", "__EOF__", "__FILE__",
    "__FILE_FULL_PATH__", "__FUNCTION__", "__gshared", "__LINE__", "__MODULE__",
    "__parameters", "__PRETTY_FUNCTION__", "__TIME__", "__TIMESTAMP__", "__traits",
    "__vector", "__VENDOR__", "__VERSION__"
];

/// Other tokens
private enum dynamicTokens = [
    "specialTokenSequence", "comment", "identifier", "scriptLine",
    "whitespace", "doubleLiteral", "floatLiteral", "idoubleLiteral",
    "ifloatLiteral", "intLiteral", "longLiteral", "realLiteral",
    "irealLiteral", "uintLiteral", "ulongLiteral", "characterLiteral",
    "dstringLiteral", "stringLiteral", "wstringLiteral"
];

private enum pseudoTokenHandlers = [
    "\"", "lexStringLiteral",
    "`", "lexWysiwygString",
    "//", "lexSlashSlashComment",
    "/*", "lexSlashStarComment",
    "/+", "lexSlashPlusComment",
    ".", "lexDot",
    "'", "lexCharacterLiteral",
    "0", "lexNumber",
    "1", "lexDecimal",
    "2", "lexDecimal",
    "3", "lexDecimal",
    "4", "lexDecimal",
    "5", "lexDecimal",
    "6", "lexDecimal",
    "7", "lexDecimal",
    "8", "lexDecimal",
    "9", "lexDecimal",
    "q\"", "lexDelimitedString",
    "q{", "lexTokenString",
    "r\"", "lexWysiwygString",
    "x\"", "lexHexString",
    " ", "lexWhitespace",
    "\t", "lexWhitespace",
    "\r", "lexWhitespace",
    "\n", "lexWhitespace",
    "\v", "lexWhitespace",
    "\f", "lexWhitespace",
    "\u2028", "lexLongNewline",
    "\u2029", "lexLongNewline",
    "#!", "lexScriptLine",
    "#line", "lexSpecialTokenSequence"
];

/// Token ID type for the D lexer.
public alias IdType = TokenIdType!(operators, dynamicTokens, keywords);

/**
 * Function used for converting an IdType to a string.
 *
 * Examples:
 * ---
 * IdType c = tok!"case";
 * assert (str(c) == "case");
 * ---
 */
public alias str = tokenStringRepresentation!(IdType, operators, dynamicTokens, keywords);

/**
 * Template used to refer to D token types.
 *
 * See the $(B operators), $(B keywords), and $(B dynamicTokens) enums for
 * values that can be passed to this template.
 * Example:
 * ---
 * import dparse.lexer;
 * IdType t = tok!"floatLiteral";
 * ---
 */
public template tok(string token)
{
    alias tok = TokenId!(IdType, operators, dynamicTokens, keywords, token);
}

private enum extraFields = q{
    string comment;
    string trailingComment;

    int opCmp(size_t i) const pure nothrow @safe {
        if (index < i) return -1;
        if (index > i) return 1;
        return 0;
    }

    int opCmp(ref const typeof(this) other) const pure nothrow @safe {
        return opCmp(other.index);
    }
};

/// The token type in the D lexer
public alias Token = std.experimental.lexer.TokenStructure!(IdType, extraFields);

/**
 * Configure whitespace handling
 */
public enum WhitespaceBehavior : ubyte
{
    include = 0b0000_0000,
    skip = 0b0000_0001,
}

/**
 * Configure string lexing behavior
 */
public enum StringBehavior : ubyte
{
    /// Do not include quote characters, process escape sequences
    compiler = 0b0000_0000,
    /// Opening quotes, closing quotes, and string suffixes are included in the
    /// string token
    includeQuoteChars = 0b0000_0001,
    /// String escape sequences are not replaced
    notEscaped = 0b0000_0010,
    /// Not modified at all. Useful for formatters or highlighters
    source = includeQuoteChars | notEscaped
}

public enum CommentBehavior : bool
{
    intern = true,
    noIntern = false
}
/**
 * Lexer configuration struct
 */
public struct LexerConfig
{
    string fileName;
    StringBehavior stringBehavior;
    WhitespaceBehavior whitespaceBehavior;
    CommentBehavior commentBehavior = CommentBehavior.intern;
}

/**
 * Basic type token types.
 */
public alias BasicTypes = AliasSeq!(tok!"int", tok!"bool", tok!"byte",
        tok!"cdouble", tok!"cent", tok!"cfloat", tok!"char", tok!"creal",
        tok!"dchar", tok!"double", tok!"float", tok!"idouble",
        tok!"ifloat", tok!"ireal", tok!"long", tok!"real", tok!"short",
        tok!"ubyte", tok!"ucent", tok!"uint", tok!"ulong", tok!"ushort",
        tok!"void", tok!"wchar");

/**
 * Returns: true if the given ID is for a basic type.
 */
public bool isBasicType(IdType type) nothrow pure @safe @nogc
{
    switch (type)
    {
    foreach (T; BasicTypes)
    {
    case T:
        return true;
    }
    default:
        return false;
    }
}

/**
 * Number literal token types.
 */
public alias NumberLiterals = AliasSeq!(tok!"doubleLiteral",
        tok!"floatLiteral", tok!"idoubleLiteral", tok!"ifloatLiteral",
        tok!"intLiteral", tok!"longLiteral", tok!"realLiteral",
        tok!"irealLiteral", tok!"uintLiteral", tok!"ulongLiteral");

/**
 * Returns: true if the given ID type is for a number literal.
 */
public bool isNumberLiteral(IdType type) nothrow pure @safe @nogc
{
    switch (type)
    {
    foreach (T; NumberLiterals)
    {
    case T:
        return true;
    }
    default:
        return false;
    }
}

/**
 * Number literal token types.
 */
public alias IntegerLiterals = AliasSeq!(tok!"intLiteral", tok!"longLiteral",
        tok!"uintLiteral", tok!"ulongLiteral");

/**
 * Returns: true if the given ID type is for a integer literal.
 */
public bool isIntegerLiteral(IdType type) nothrow pure @safe @nogc
{
    switch (type)
    {
    foreach (T; IntegerLiterals)
    {
    case T:
        return true;
    }
    default:
        return false;
    }
}

/**
 * Operator token types.
 */
public alias Operators = AliasSeq!(tok!",", tok!".", tok!"..", tok!"...",
        tok!"/", tok!"/=", tok!"!", tok!"!<", tok!"!<=", tok!"!<>",
        tok!"!<>=", tok!"!=", tok!"!>", tok!"!>=", tok!"$", tok!"%",
        tok!"%=", tok!"&", tok!"&&", tok!"&=", tok!"(", tok!")",
        tok!"*", tok!"*=", tok!"+", tok!"++", tok!"+=", tok!"-",
        tok!"--", tok!"-=", tok!":", tok!";", tok!"<", tok!"<<",
        tok!"<<=", tok!"<=", tok!"<>", tok!"<>=", tok!"=", tok!"==",
        tok!"=>", tok!">", tok!">=", tok!">>", tok!">>=", tok!">>>",
        tok!">>>=", tok!"?", tok!"@", tok!"[", tok!"]", tok!"^",
        tok!"^=", tok!"^^", tok!"^^=", tok!"{", tok!"|", tok!"|=",
        tok!"||", tok!"}", tok!"~", tok!"~=");

/**
 * Returns: true if the given ID type is for an operator.
 */
public bool isOperator(IdType type) nothrow pure @safe @nogc
{
    switch (type)
    {
    foreach (T; Operators)
    {
    case T:
        return true;
    }
    default:
        return false;
    }
}

/**
 * Keyword token types.
 */
public alias Keywords = AliasSeq!(tok!"abstract", tok!"alias", tok!"align",
        tok!"asm", tok!"assert", tok!"auto", tok!"break",
        tok!"case", tok!"cast", tok!"catch", tok!"class", tok!"const",
        tok!"continue", tok!"debug", tok!"default", tok!"delegate",
        tok!"delete", tok!"deprecated", tok!"do", tok!"else", tok!"enum",
        tok!"export", tok!"extern", tok!"false", tok!"final", tok!"finally",
        tok!"for", tok!"foreach", tok!"foreach_reverse", tok!"function",
        tok!"goto", tok!"if", tok!"immutable", tok!"import", tok!"in",
        tok!"inout", tok!"interface", tok!"invariant", tok!"is",
        tok!"lazy", tok!"macro", tok!"mixin", tok!"module", tok!"new",
        tok!"nothrow", tok!"null", tok!"out", tok!"override", tok!"package",
        tok!"pragma", tok!"private", tok!"protected", tok!"public",
        tok!"pure", tok!"ref", tok!"return", tok!"scope", tok!"shared",
        tok!"static", tok!"struct", tok!"super", tok!"switch", tok!"synchronized",
        tok!"template", tok!"this", tok!"throw", tok!"true", tok!"try",
        tok!"typedef", tok!"typeid", tok!"typeof", tok!"union", tok!"unittest",
        tok!"version", tok!"while", tok!"with", tok!"__DATE__",
        tok!"__EOF__", tok!"__FILE__", tok!"__FILE_FULL_PATH__", tok!"__FUNCTION__",
        tok!"__gshared", tok!"__LINE__", tok!"__MODULE__", tok!"__parameters",
        tok!"__PRETTY_FUNCTION__", tok!"__TIME__", tok!"__TIMESTAMP__",
        tok!"__traits", tok!"__vector", tok!"__VENDOR__", tok!"__VERSION__");

/**
 * Returns: true if the given ID type is for a keyword.
 */
public bool isKeyword(IdType type) pure nothrow @safe @nogc
{
    switch (type)
    {
    foreach (T; Keywords)
    {
    case T:
        return true;
    }
    default:
        return false;
    }
}

/**
 * String literal token types
 */
public alias StringLiterals = AliasSeq!(tok!"dstringLiteral",
        tok!"stringLiteral", tok!"wstringLiteral");

/**
 * Returns: true if the given ID type is for a string literal.
 */
public bool isStringLiteral(IdType type) pure nothrow @safe @nogc
{
    switch (type)
    {
    foreach (T; StringLiterals)
    {
    case T:
        return true;
    }
    default:
        return false;
    }
}

/**
 * Protection token types.
 */
public alias Protections = AliasSeq!(tok!"export", tok!"package",
        tok!"private", tok!"public", tok!"protected");

/**
 * Returns: true if the given ID type is for a protection attribute.
 */
public bool isProtection(IdType type) pure nothrow @safe @nogc
{
    switch (type)
    {
    foreach (T; Protections)
    {
    case T:
        return true;
    }
    default:
        return false;
    }
}

public alias SpecialTokens = AliasSeq!(tok!"__DATE__", tok!"__TIME__",
    tok!"__TIMESTAMP__", tok!"__VENDOR__", tok!"__VERSION__", tok!"__FILE__",
    tok!"__FILE_FULL_PATH__", tok!"__LINE__", tok!"__MODULE__",
    tok!"__FUNCTION__", tok!"__PRETTY_FUNCTION__");

public bool isSpecialToken(IdType type) pure nothrow @safe @nogc
{
    switch (type)
    {
    foreach (T; SpecialTokens)
    {
    case T:
        return true;
    }
    default:
        return false;
    }
}

public alias Literals = AliasSeq!(StringLiterals, NumberLiterals, tok!"characterLiteral",
        SpecialTokens, tok!"true", tok!"false", tok!"null", tok!"$");

public bool isLiteral(IdType type) pure nothrow @safe @nogc
{
    switch (type)
    {
    foreach (T; Literals)
    {
    case T:
        return true;
    }
    default:
        return false;
    }
}

/**
 * Returns: an array of tokens lexed from the given source code to the output range. All
 * whitespace tokens are skipped and comments are attached to the token nearest
 * to them.
 */
const(Token)[] getTokensForParser(R)(R sourceCode, LexerConfig config, StringCache* cache)
if (is(Unqual!(ElementEncodingType!R) : ubyte) && isDynamicArray!R)
{
    enum CommentType : ubyte
    {
        notDoc,
        line,
        block
    }

    static CommentType commentType(string comment) pure nothrow @safe
    {
        if (comment.length < 3)
            return CommentType.notDoc;
        if (comment[0 ..3] == "///")
            return CommentType.line;
        if (comment[0 ..3] == "/++" || comment[0 ..3] == "/**")
            return CommentType.block;
        return CommentType.notDoc;
    }

    config.whitespaceBehavior = WhitespaceBehavior.skip;
    config.commentBehavior = CommentBehavior.noIntern;

    auto leadingCommentAppender = appender!(char[])();
    leadingCommentAppender.reserve(1024);
    auto trailingCommentAppender = appender!(char[])();
    trailingCommentAppender.reserve(1024);
    bool hadDdoc;
    string empty = cache.intern("");
    auto output = appender!(typeof(return))();
    auto lexer = DLexer(sourceCode, config, cache);
    size_t tokenCount;
    loop: while (!lexer.empty) switch (lexer.front.type)
    {
    case tok!"specialTokenSequence":
    case tok!"whitespace":
        lexer.popFront();
        break;
    case tok!"comment":
        final switch (commentType(lexer.front.text))
        {
        case CommentType.block:
        case CommentType.line:
            if (tokenCount > 0 && lexer.front.line == output.data[tokenCount - 1].line)
            {
                if (!trailingCommentAppender.data.empty)
                    trailingCommentAppender.put('\n');
                unDecorateComment(lexer.front.text, trailingCommentAppender);
                hadDdoc = true;
            }
            else
            {
                if (!leadingCommentAppender.data.empty)
                    leadingCommentAppender.put('\n');
                unDecorateComment(lexer.front.text, leadingCommentAppender);
                hadDdoc = true;
            }
            lexer.popFront();
            break;
        case CommentType.notDoc:
            lexer.popFront();
            break;
        }
        break;
    case tok!"__EOF__":
        if (!trailingCommentAppender.data.empty)
            (cast() output.data[$ - 1].trailingComment) = cache.intern(cast(string) trailingCommentAppender.data);
        break loop;
    default:
        Token t = lexer.front;
        lexer.popFront();
        tokenCount++;
        if (!output.data.empty && !trailingCommentAppender.data.empty)
        {
            (cast() output.data[$ - 1].trailingComment) =
                cache.intern(cast(string) trailingCommentAppender.data);
            hadDdoc = false;
        }
        t.comment = leadingCommentAppender.data.length > 0
            ? cache.intern(cast(string) leadingCommentAppender.data) : (hadDdoc ? empty : null);
        leadingCommentAppender.clear();
        trailingCommentAppender.clear();
        hadDdoc = false;
        output.put(t);
        break;
    }
    return output.data;
}

/**
 * The D lexer struct.
 */
public struct DLexer
{
    mixin Lexer!(Token, lexIdentifier, isSeparating, operators, dynamicTokens,
        keywords, pseudoTokenHandlers);

    ///
    @disable this();

    /**
     * Params:
     *     range = the bytes that compose the source code that will be lexed.
     *     config = the lexer configuration to use.
     *     cache = the string interning cache for de-duplicating identifiers and
     *         other token text.
     *     haveSSE42 = Parse streaming SIMD Extensions 4.2 in inline assembly
     */
    this(R)(R range, const LexerConfig config, StringCache* cache,
        bool haveSSE42 = sse42()) pure nothrow @safe
    if (is(Unqual!(ElementEncodingType!R) : ubyte) && isDynamicArray!R)
    {
        this.haveSSE42 = haveSSE42;
        auto r = (range.length >= 3 && range[0] == 0xef && range[1] == 0xbb && range[2] == 0xbf)
            ? range[3 .. $] : range;
        this.range = LexerRange(cast(const(ubyte)[]) r);
        this.config = config;
        this.cache = cache;
        popFront();
    }

    ///
    public void popFront()() pure nothrow @safe
    {
        do
            _popFront();
        while (config.whitespaceBehavior == WhitespaceBehavior.skip
            && _front.type == tok!"whitespace");
    }

private pure nothrow @safe:

    bool isWhitespace()
    {
        switch (range.bytes[range.index])
        {
        case ' ':
        case '\r':
        case '\n':
        case '\t':
        case '\v':
        case '\f':
            return true;
        case 0xe2:
            auto peek = range.peek(2);
            return peek.length == 2
                && peek[0] == 0x80
                && (peek[1] == 0xa8 || peek[1] == 0xa9);
        default:
            return false;
        }
    }

    void popFrontWhitespaceAware()
    {
        switch (range.bytes[range.index])
        {
        case '\r':
            range.popFront();
            if (!(range.index >= range.bytes.length) && range.bytes[range.index] == '\n')
            {
                range.popFront();
                range.incrementLine();
            }
            else
                range.incrementLine();
            return;
        case '\n':
            range.popFront();
            range.incrementLine();
            return;
        case 0xe2:
            auto lookahead = range.peek(3);
            if (lookahead.length == 3 && lookahead[1] == 0x80
                && (lookahead[2] == 0xa8 || lookahead[2] == 0xa9))
            {
                range.index+=3;
                range.column+=3;
                range.incrementLine();
                return;
            }
            else
            {
                range.popFront();
                return;
            }
        default:
            range.popFront();
            return;
        }
    }

    void lexWhitespace(ref Token token) @trusted
    {
        mixin (tokenStart);
        loop: do
        {
            version (iasm64NotWindows)
            {
                if (haveSSE42 && range.index + 16 < range.bytes.length)
                {
                    skip!(true, '\t', ' ', '\v', '\f')(range.bytes.ptr + range.index,
                        &range.index, &range.column);
                }
            }
            switch (range.bytes[range.index])
            {
            case '\r':
                range.popFront();
                if (!(range.index >= range.bytes.length) && range.bytes[range.index] == '\n')
                {
                    range.popFront();
                }
                range.column = 1;
                range.line += 1;
                break;
            case '\n':
                range.popFront();
                range.column = 1;
                range.line += 1;
                break;
            case ' ':
            case '\t':
            case '\v':
            case '\f':
                range.popFront();
                break;
            case 0xe2:
                if (range.index + 2 >= range.bytes.length)
                    break loop;
                if (range.bytes[range.index + 1] != 0x80)
                    break loop;
                if (range.bytes[range.index + 2] == 0xa8 || range.bytes[range.index + 2] == 0xa9)
                {
                    range.index += 3;
                    range.column += 3;
                    range.column = 1;
                    range.line += 1;
                    break;
                }
                break loop;
            default:
                break loop;
            }
        } while (!(range.index >= range.bytes.length));
        string text = config.whitespaceBehavior == WhitespaceBehavior.include
            ? cache.intern(range.slice(mark)) : "";
        token = Token(tok!"whitespace", text, line, column, index);
    }

    void lexNumber(ref Token token)
    {
        mixin (tokenStart);
        if (range.bytes[range.index] == '0' && range.index + 1 < range.bytes.length)
        {
            immutable ahead = range.bytes[range.index + 1];
            switch (ahead)
            {
            case 'x':
            case 'X':
                range.index += 2;
                range.column += 2;
                lexHex(token, mark, line, column, index);
                return;
            case 'b':
            case 'B':
                range.index += 2;
                range.column += 2;
                lexBinary(token, mark, line, column, index);
                return;
            default:
                lexDecimal(token, mark, line, column, index);
                return;
            }
        }
        else
            lexDecimal(token, mark, line, column, index);
    }

    void lexHex(ref Token token)
    {
        mixin (tokenStart);
        lexHex(token, mark, line, column, index);
    }

    void lexHex(ref Token token, size_t mark, size_t line, size_t column,
        size_t index) @trusted
    {
        IdType type = tok!"intLiteral";
        bool foundDot;
        hexLoop: while (!(range.index >= range.bytes.length))
        {
            switch (range.bytes[range.index])
            {
            case 'a': .. case 'f':
            case 'A': .. case 'F':
            case '0': .. case '9':
            case '_':
                version (iasm64NotWindows)
                {
                    if (haveSSE42 && range.index + 16 < range.bytes.length)
                    {
                        immutable ulong i = rangeMatch!(false, '0', '9', 'a', 'f', 'A', 'F', '_', '_')
                            (range.bytes.ptr + range.index);
                        range.column += i;
                        range.index += i;
                    }
                    else
                        range.popFront();
                }
                else
                    range.popFront();
                break;
            case 'u':
            case 'U':
                lexIntSuffix(type);
                break hexLoop;
            case 'i':
                if (foundDot)
                    lexFloatSuffix(type);
                break hexLoop;
            case 'L':
                if (foundDot)
                    lexFloatSuffix(type);
                else
                    lexIntSuffix(type);
                break hexLoop;
            case 'p':
            case 'P':
                lexExponent(type);
                break hexLoop;
            case '.':
                if (foundDot || !(range.index + 1 < range.bytes.length) || range.peekAt(1) == '.')
                    break hexLoop;
                else
                {
                    // The following bit of silliness tries to tell the
                    // difference between "int dot identifier" and
                    // "double identifier".
                    if (range.index + 1 < range.bytes.length)
                    {
                        switch (range.peekAt(1))
                        {
                        case '0': .. case '9':
                        case 'A': .. case 'F':
                        case 'a': .. case 'f':
                            goto doubleLiteral;
                        default:
                            break hexLoop;
                        }
                    }
                    else
                    {
                    doubleLiteral:
                        range.popFront();
                        foundDot = true;
                        type = tok!"doubleLiteral";
                    }
                }
                break;
            default:
                break hexLoop;
            }
        }
        token = Token(type, cache.intern(range.slice(mark)), line, column,
            index);
    }

    void lexBinary(ref Token token)
    {
        mixin (tokenStart);
        return lexBinary(token, mark, line, column, index);
    }

    void lexBinary(ref Token token, size_t mark, size_t line, size_t column,
        size_t index) @trusted
    {
        IdType type = tok!"intLiteral";
        binaryLoop: while (!(range.index >= range.bytes.length))
        {
            switch (range.bytes[range.index])
            {
            case '0':
            case '1':
            case '_':
                version (iasm64NotWindows)
                {
                    if (haveSSE42 && range.index + 16 < range.bytes.length)
                    {
                        immutable ulong i = rangeMatch!(false, '0', '1', '_', '_')(
                            range.bytes.ptr + range.index);
                        range.column += i;
                        range.index += i;
                    }
                    else
                        range.popFront();
                }
                else
                    range.popFront();
                break;
            case 'u':
            case 'U':
            case 'L':
                lexIntSuffix(type);
                break binaryLoop;
            default:
                break binaryLoop;
            }
        }
        token = Token(type, cache.intern(range.slice(mark)), line, column,
            index);
    }

    void lexDecimal(ref Token token)
    {
        mixin (tokenStart);
        lexDecimal(token, mark, line, column, index);
    }

    void lexDecimal(ref Token token, size_t mark, size_t line, size_t column,
        size_t index) @trusted
    {
        bool foundDot = range.bytes[range.index] == '.';
        IdType type = tok!"intLiteral";
        if (foundDot)
        {
            range.popFront();
            type = tok!"doubleLiteral";
        }

        decimalLoop: while (!(range.index >= range.bytes.length))
        {
            switch (range.bytes[range.index])
            {
            case '0': .. case '9':
            case '_':
                version (iasm64NotWindows)
                {
                    if (haveSSE42 && range.index + 16 < range.bytes.length)
                    {
                        immutable ulong i = rangeMatch!(false, '0', '9', '_', '_')(range.bytes.ptr + range.index);
                        range.column += i;
                        range.index += i;
                    }
                    else
                        range.popFront();
                }
                else
                    range.popFront();
                break;
            case 'u':
            case 'U':
                if (!foundDot)
                    lexIntSuffix(type);
                break decimalLoop;
            case 'i':
                lexFloatSuffix(type);
                break decimalLoop;
            case 'L':
                if (foundDot)
                    lexFloatSuffix(type);
                else
                    lexIntSuffix(type);
                break decimalLoop;
            case 'f':
            case 'F':
                lexFloatSuffix(type);
                break decimalLoop;
            case 'e':
            case 'E':
                lexExponent(type);
                break decimalLoop;
            case '.':
                if (foundDot || !(range.index + 1 < range.bytes.length) || range.peekAt(1) == '.')
                    break decimalLoop;
                else
                {
                    // The following bit of silliness tries to tell the
                    // difference between "int dot identifier" and
                    // "double identifier".
                    if (range.index + 1 < range.bytes.length)
                    {
                        immutable ch = range.peekAt(1);
                        if (ch <= 0x2f
                            || (ch >= '0' && ch <= '9')
                            || (ch >= ':' && ch <= '@')
                            || (ch >= '[' && ch <= '^')
                            || (ch >= '{' && ch <= '~')
                            || ch == '`' || ch == '_')
                        {
                            goto doubleLiteral;
                        }
                        else
                            break decimalLoop;
                    }
                    else
                    {
                    doubleLiteral:
                        range.popFront();
                        foundDot = true;
                        type = tok!"doubleLiteral";
                    }
                }
                break;
            default:
                break decimalLoop;
            }
        }
        token = Token(type, cache.intern(range.slice(mark)), line, column,
            index);
    }

    void lexIntSuffix(ref IdType type) pure nothrow @safe
    {
        bool secondPass;
        if (range.bytes[range.index] == 'u' || range.bytes[range.index] == 'U')
        {
    U:
            if (type == tok!"intLiteral")
                type = tok!"uintLiteral";
            else
                type = tok!"ulongLiteral";
            range.popFront();
            if (secondPass)
                return;
            if (range.index < range.bytes.length
                    && (range.bytes[range.index] == 'L' || range.bytes[range.index] == 'l'))
                goto L;
            goto I;
        }
        if (range.bytes[range.index] == 'L' || range.bytes[range.index] == 'l')
        {
    L:
            if (type == tok!"uintLiteral")
                type = tok!"ulongLiteral";
            else
                type = tok!"longLiteral";
            range.popFront();
            if (range.index < range.bytes.length
                    && (range.bytes[range.index] == 'U' || range.bytes[range.index] == 'u'))
            {
                secondPass = true;
                goto U;
            }
            goto I;
        }
    I:
        if (range.index < range.bytes.length && range.bytes[range.index] == 'i')
        {
            warning("Complex number literals are deprecated");
            range.popFront();
            if (type == tok!"longLiteral" || type == tok!"ulongLiteral")
                type = tok!"idoubleLiteral";
            else
                type = tok!"ifloatLiteral";
        }
    }

    void lexFloatSuffix(ref IdType type) pure nothrow @safe
    {
        switch (range.bytes[range.index])
        {
        case 'L':
            range.popFront();
            type = tok!"doubleLiteral";
            break;
        case 'f':
        case 'F':
            range.popFront();
            type = tok!"floatLiteral";
            break;
        default:
            break;
        }
        if (range.index < range.bytes.length && range.bytes[range.index] == 'i')
        {
            warning("Complex number literals are deprecated");
            range.popFront();
            if (type == tok!"floatLiteral")
                type = tok!"ifloatLiteral";
            else
                type = tok!"idoubleLiteral";
        }
    }

    void lexExponent(ref IdType type) pure nothrow @safe
    {
        range.popFront();
        bool foundSign = false;
        bool foundDigit = false;
        while (range.index < range.bytes.length)
        {
            switch (range.bytes[range.index])
            {
            case '-':
            case '+':
                if (foundSign)
                {
                    if (!foundDigit)
                    error("Expected an exponent");
                    return;
                }
                foundSign = true;
                range.popFront();
                break;
            case '0': .. case '9':
            case '_':
                foundDigit = true;
                range.popFront();
                break;
            case 'L':
            case 'f':
            case 'F':
            case 'i':
                lexFloatSuffix(type);
                return;
            default:
                if (!foundDigit)
                    error("Expected an exponent");
                return;
            }
        }
    }

    void lexScriptLine(ref Token token)
    {
        mixin (tokenStart);
        while (!(range.index >= range.bytes.length) && !isNewline)
        {
            range.popFront();
        }
        token = Token(tok!"scriptLine", cache.intern(range.slice(mark)),
            line, column, index);
    }

    void lexSpecialTokenSequence(ref Token token)
    {
        mixin (tokenStart);
        while (!(range.index >= range.bytes.length) && !isNewline)
        {
            range.popFront();
        }
        token = Token(tok!"specialTokenSequence", cache.intern(range.slice(mark)),
            line, column, index);
    }

    void lexSlashStarComment(ref Token token) @trusted
    {
        mixin (tokenStart);
        IdType type = tok!"comment";
        range.popFrontN(2);
        while (range.index < range.bytes.length)
        {
            version (iasm64NotWindows)
            {
                if (haveSSE42 && range.index + 16 < range.bytes.length)
                    skip!(false, '\r', '\n', '/', '*', 0xe2)(range.bytes.ptr + range.index,
                        &range.index, &range.column);
            }
            if (range.bytes[range.index] == '*')
            {
                range.popFront();
                if (!(range.index >= range.bytes.length) && range.bytes[range.index] == '/')
                {
                    range.popFront();
                    break;
                }
            }
            else
                popFrontWhitespaceAware();
        }
        if (config.commentBehavior == CommentBehavior.intern)
            token = Token(type, cache.intern(range.slice(mark)), line, column, index);
        else
            token = Token(type, cast(string) range.slice(mark), line, column, index);
    }

    void lexSlashSlashComment(ref Token token) @trusted
    {
        mixin (tokenStart);
        IdType type = tok!"comment";
        range.popFrontN(2);
        while (range.index < range.bytes.length)
        {
            version (iasm64NotWindows)
            {
                if (haveSSE42 && range.index + 16 < range.bytes.length)
                {
                    skip!(false, '\r', '\n', 0xe2)(range.bytes.ptr + range.index,
                        &range.index, &range.column);
                }
            }
            if (range.bytes[range.index] == '\r' || range.bytes[range.index] == '\n')
                break;
            range.popFront();
        }
        if (config.commentBehavior == CommentBehavior.intern)
            token = Token(type, cache.intern(range.slice(mark)), line, column, index);
        else
            token = Token(type, cast(string) range.slice(mark), line, column, index);
    }

    void lexSlashPlusComment(ref Token token) @trusted
    {
        mixin (tokenStart);
        IdType type = tok!"comment";
        range.index += 2;
        range.column += 2;
        int depth = 1;
        while (depth > 0 && !(range.index >= range.bytes.length))
        {
            version (iasm64NotWindows)
            {
                if (haveSSE42 && range.index + 16 < range.bytes.length)
                {
                    skip!(false, '+', '/', '\\', '\r', '\n', 0xe2)(range.bytes.ptr + range.index,
                        &range.index, &range.column);
                }
            }
            if (range.bytes[range.index] == '+')
            {
                range.popFront();
                if (!(range.index >= range.bytes.length) && range.bytes[range.index] == '/')
                {
                    range.popFront();
                    depth--;
                }
            }
            else if (range.bytes[range.index] == '/')
            {
                range.popFront();
                if (!(range.index >= range.bytes.length) && range.bytes[range.index] == '+')
                {
                    range.popFront();
                    depth++;
                }
            }
            else
                popFrontWhitespaceAware();
        }
        if (config.commentBehavior == CommentBehavior.intern)
            token = Token(type, cache.intern(range.slice(mark)), line, column, index);
        else
            token = Token(type, cast(string) range.slice(mark), line, column, index);
    }

    void lexStringLiteral(ref Token token) @trusted
    {
        mixin (tokenStart);
        range.popFront();
        while (true)
        {
            if (range.index >= range.bytes.length)
            {
                error("Error: unterminated string literal");
                token = Token(tok!"");
                return;
            }
            version (iasm64NotWindows)
            {
                if (haveSSE42 && range.index + 16 < range.bytes.length)
                {
                    skip!(false, '"', '\\', '\r', '\n', 0xe2)(range.bytes.ptr + range.index,
                        &range.index, &range.column);
                }
            }
            if (range.bytes[range.index] == '"')
            {
                range.popFront();
                break;
            }
            else if (range.bytes[range.index] == '\\')
            {
                lexEscapeSequence();
            }
            else
                popFrontWhitespaceAware();
        }
        IdType type = tok!"stringLiteral";
        lexStringSuffix(type);
        token = Token(type, cache.intern(range.slice(mark)), line, column,
            index);
    }

    void lexWysiwygString(ref Token token) @trusted
    {
        mixin (tokenStart);
        IdType type = tok!"stringLiteral";
        immutable bool backtick = range.bytes[range.index] == '`';
        if (backtick)
        {
            range.popFront();
            while (true)
            {
                if (range.index >= range.bytes.length)
                {
                    error("Error: unterminated string literal");
                    token = Token(tok!"");
                    return;
                }
                version (iasm64NotWindows)
                {
                    if (haveSSE42 && range.index + 16 < range.bytes.length)
                    {
                        skip!(false, '\r', '\n', 0xe2, '`')(range.bytes.ptr + range.index,
                            &range.index, &range.column);
                    }
                }
                if (range.bytes[range.index] == '`')
                {
                    range.popFront();
                    break;
                }
                else
                    popFrontWhitespaceAware();
            }
        }
        else
        {
            range.popFront();
            if (range.index >= range.bytes.length)
            {
                error("Error: unterminated string literal");
                token = Token(tok!"");
                return;
            }
            range.popFront();
            while (true)
            {
                if (range.index >= range.bytes.length)
                {
                    error("Error: unterminated string literal");
                    token = Token(tok!"");
                    return;
                }
                else if (range.bytes[range.index] == '"')
                {
                    range.popFront();
                    break;
                }
                else
                    popFrontWhitespaceAware();
            }
        }
        lexStringSuffix(type);
        token = Token(type, cache.intern(range.slice(mark)), line, column,
            index);
    }

    private ubyte lexStringSuffix(ref IdType type) pure nothrow @safe
    {
        if (range.index >= range.bytes.length)
        {
            type = tok!"stringLiteral";
            return 0;
        }
        else
        {
            switch (range.bytes[range.index])
            {
            case 'w': range.popFront(); type = tok!"wstringLiteral"; return 'w';
            case 'd': range.popFront(); type = tok!"dstringLiteral"; return 'd';
            case 'c': range.popFront(); type = tok!"stringLiteral"; return 'c';
            default: type = tok!"stringLiteral"; return 0;
            }
        }
    }

    void lexDelimitedString(ref Token token)
    {
        mixin (tokenStart);
        range.index += 2;
        range.column += 2;
        ubyte open;
        ubyte close;
        switch (range.bytes[range.index])
        {
        case '<':
            open = '<';
            close = '>';
            range.popFront();
            lexNormalDelimitedString(token, mark, line, column, index, open, close);
            break;
        case '{':
            open = '{';
            close = '}';
            range.popFront();
            lexNormalDelimitedString(token, mark, line, column, index, open, close);
            break;
        case '[':
            open = '[';
            close = ']';
            range.popFront();
            lexNormalDelimitedString(token, mark, line, column, index, open, close);
            break;
        case '(':
            open = '(';
            close = ')';
            range.popFront();
            lexNormalDelimitedString(token, mark, line, column, index, open, close);
            break;
        default:
            lexHeredocString(token, mark, line, column, index);
            break;
        }
    }

    void lexNormalDelimitedString(ref Token token, size_t mark, size_t line, size_t column,
        size_t index, ubyte open, ubyte close)
    {
        int depth = 1;
        while (!(range.index >= range.bytes.length) && depth > 0)
        {
            if (range.bytes[range.index] == open)
            {
                depth++;
                range.popFront();
            }
            else if (range.bytes[range.index] == close)
            {
                depth--;
                range.popFront();
                if (depth <= 0)
                {
                    if (range.bytes[range.index] == '"')
                    {
                        range.popFront();
                    }
                    else
                    {
                        error("Error: `\"` expected to end delimited string literal");
                        token = Token(tok!"");
                        return;
                    }
                }
            }
            else
                popFrontWhitespaceAware();
        }
        IdType type = tok!"stringLiteral";
        lexStringSuffix(type);
        token = Token(type, cache.intern(range.slice(mark)), line, column, index);
    }

    void lexHeredocString(ref Token token, size_t mark, size_t line, size_t column, size_t index)
    {
        Token ident;
        lexIdentifier(ident);
        if (isNewline())
            popFrontWhitespaceAware();
        else
            error("Newline expected");
        while (!(range.index >= range.bytes.length))
        {
            if (isNewline())
            {
                popFrontWhitespaceAware();
                if (!range.canPeek(ident.text.length))
                {
                    error(ident.text ~ " expected");
                    break;
                }
                if (range.peek(ident.text.length - 1) == ident.text)
                {
                    range.popFrontN(ident.text.length);
                    break;
                }
            }
            else
            {
                range.popFront();
            }
        }
        if (!(range.index >= range.bytes.length) && range.bytes[range.index] == '"')
        {
            range.popFront();
        }
        else
            error("`\"` expected");
        IdType type = tok!"stringLiteral";
        lexStringSuffix(type);
        token = Token(type, cache.intern(range.slice(mark)), line, column, index);
    }

    void lexTokenString(ref Token token)
    {
        mixin (tokenStart);
        assert (range.bytes[range.index] == 'q');
        range.popFront();
        assert (range.bytes[range.index] == '{');
        range.popFront();
        auto app = appender!string();
        app.put("q{");
        int depth = 1;

        immutable WhitespaceBehavior oldWhitespace = config.whitespaceBehavior;
        immutable StringBehavior oldString = config.stringBehavior;
        config.whitespaceBehavior = WhitespaceBehavior.include;
        config.stringBehavior = StringBehavior.source;
        scope (exit)
        {
            config.whitespaceBehavior = oldWhitespace;
            config.stringBehavior = oldString;
        }

        advance(_front);
        while (depth > 0 && !empty)
        {
            auto t = front();
            if (t.text is null)
                app.put(str(t.type));
            else
                app.put(t.text);
            if (t.type == tok!"}")
            {
                depth--;
                if (depth > 0)
                popFront();
            }
            else if (t.type == tok!"{")
            {
                depth++;
                popFront();
            }
            else
                popFront();
        }
        IdType type = tok!"stringLiteral";
        auto b = lexStringSuffix(type);
        if (b != 0)
            app.put(b);
        token = Token(type, cache.intern(cast(const(ubyte)[]) app.data), line,
            column, index);
    }

    void lexHexString(ref Token token)
    {
        mixin (tokenStart);
        range.index += 2;
        range.column += 2;

        loop: while (true)
        {
            if (range.index >= range.bytes.length)
            {
                error("Error: unterminated hex string literal");
                token = Token(tok!"");
                return;
            }
            else if (isWhitespace())
                popFrontWhitespaceAware();
            else switch (range.bytes[range.index])
            {
            case '0': .. case '9':
            case 'A': .. case 'F':
            case 'a': .. case 'f':
                range.popFront();
                break;
            case '"':
                range.popFront();
                break loop;
            default:
                error("Error: invalid character in hex string");
                token = Token(tok!"");
                return;
            }
        }

        IdType type = tok!"stringLiteral";
        lexStringSuffix(type);
        token = Token(type, cache.intern(range.slice(mark)), line, column,
            index);
    }

    bool lexEscapeSequence()
    {
        range.popFront();
        if (range.index >= range.bytes.length)
        {
            error("Error: non-terminated character escape sequence.");
            return false;
        }
        switch (range.bytes[range.index])
        {
        case '\'':
        case '"':
        case '?':
        case '\\':
        case 'a':
        case 'b':
        case 'f':
        case 'n':
        case 'r':
        case 't':
        case 'v':
            range.popFront();
            break;
        case 'x':
            range.popFront();
            foreach (i; 0 .. 2)
            {
                if (range.index >= range.bytes.length)
                {
                    error("Error: 2 hex digits expected.");
                    return false;
                }
                switch (range.bytes[range.index])
                {
                case '0': .. case '9':
                case 'a': .. case 'f':
                case 'A': .. case 'F':
                    range.popFront();
                    break;
                default:
                    error("Error: 2 hex digits expected.");
                    return false;
                }
            }
            break;
        case '0':
            if (!(range.index + 1 < range.bytes.length)
                || ((range.index + 1 < range.bytes.length) && range.peekAt(1) == '\''))
            {
                range.popFront();
                break;
            }
            goto case;
        case '1': .. case '7':
            for (size_t i = 0; i < 3 && !(range.index >= range.bytes.length)
                    && range.bytes[range.index] >= '0' && range.bytes[range.index] <= '7'; i++)
                range.popFront();
            break;
        case 'u':
            range.popFront();
            foreach (i; 0 .. 4)
            {
                if (range.index >= range.bytes.length)
                {
                    error("Error: at least 4 hex digits expected.");
                    return false;
                }
                switch (range.bytes[range.index])
                {
                case '0': .. case '9':
                case 'a': .. case 'f':
                case 'A': .. case 'F':
                    range.popFront();
                    break;
                default:
                    error("Error: at least 4 hex digits expected.");
                    return false;
                }
            }
            break;
        case 'U':
            range.popFront();
            foreach (i; 0 .. 8)
            {
                if (range.index >= range.bytes.length)
                {
                    error("Error: at least 8 hex digits expected.");
                    return false;
                }
                switch (range.bytes[range.index])
                {
                case '0': .. case '9':
                case 'a': .. case 'f':
                case 'A': .. case 'F':
                    range.popFront();
                    break;
                default:
                    error("Error: at least 8 hex digits expected.");
                    return false;
                }
            }
            break;
        default:
            while (true)
            {
                if (range.index >= range.bytes.length)
                {
                    error("Error: non-terminated character escape sequence.");
                    return false;
                }
                if (range.bytes[range.index] == ';')
                {
                    range.popFront();
                    break;
                }
                else
                {
                    range.popFront();
                }
            }
        }
        return true;
    }

    void lexCharacterLiteral(ref Token token)
    {
        mixin (tokenStart);
        range.popFront();
        if (range.empty)
            goto err;
        if (range.bytes[range.index] == '\\')
            lexEscapeSequence();
        else if (range.bytes[range.index] == '\'')
        {
            range.popFront();
            token = Token(tok!"characterLiteral", cache.intern(range.slice(mark)),
                line, column, index);
        }
        else if (range.bytes[range.index] & 0x80)
        {
            while (range.bytes[range.index] & 0x80)
                range.popFront();
        }
        else
            popFrontWhitespaceAware();

        if (range.index < range.bytes.length && range.bytes[range.index] == '\'')
        {
            range.popFront();
            token = Token(tok!"characterLiteral", cache.intern(range.slice(mark)),
                line, column, index);
        }
        else
        {
    err:
            error("Error: Expected `'` to end character literal");
            token = Token(tok!"");
        }
    }

    void lexIdentifier(ref Token token) @trusted
    {
        mixin (tokenStart);
        if (isSeparating(0))
        {
            error("Invalid identifier");
            range.popFront();
        }
        while (true)
        {
            version (iasm64NotWindows)
            {
                if (haveSSE42 && range.index + 16 < range.bytes.length)
                {
                    immutable ulong i = rangeMatch!(false, 'a', 'z', 'A', 'Z', '_', '_')
                        (range.bytes.ptr + range.index);
                    range.column += i;
                    range.index += i;
                }
            }
            if (isSeparating(0))
                break;
            else
                range.popFront();
        }
        token = Token(tok!"identifier", cache.intern(range.slice(mark)), line,
            column, index);
    }

    void lexDot(ref Token token)
    {
        mixin (tokenStart);
        if (!(range.index + 1 < range.bytes.length))
        {
            range.popFront();
            token = Token(tok!".", null, line, column, index);
            return;
        }
        switch (range.peekAt(1))
        {
        case '0': .. case '9':
            lexNumber(token);
            return;
        case '.':
            range.popFront();
            range.popFront();
            if (!(range.index >= range.bytes.length) && range.bytes[range.index] == '.')
            {
                range.popFront();
                token = Token(tok!"...", null, line, column, index);
            }
            else
                token = Token(tok!"..", null, line, column, index);
            return;
        default:
            range.popFront();
            token = Token(tok!".", null, line, column, index);
            return;
        }
    }

    void lexLongNewline(ref Token token) @nogc
    {
        mixin (tokenStart);
        range.popFront();
        range.popFront();
        range.popFront();
        range.incrementLine();
        string text = config.whitespaceBehavior == WhitespaceBehavior.include
            ? cache.intern(range.slice(mark)) : "";
        token = Token(tok!"whitespace", text, line,
            column, index);
    }

    bool isNewline() @nogc
    {
        if (range.bytes[range.index] == '\n') return true;
        if (range.bytes[range.index] == '\r') return true;
        return (range.bytes[range.index] & 0x80) && (range.index + 2 < range.bytes.length)
            && (range.peek(2) == "\u2028" || range.peek(2) == "\u2029");
    }

    bool isSeparating(size_t offset) @nogc
    {
        enum : ubyte
        {
            n, y, m // no, yes, maybe
        }

        if (range.index + offset >= range.bytes.length)
            return true;
        auto c = range.bytes[range.index + offset];
        static immutable ubyte[256] LOOKUP_TABLE = [
            y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
            y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
            y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
            n, n, n, n, n, n, n, n, n, n, y, y, y, y, y, y,
            y, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n,
            n, n, n, n, n, n, n, n, n, n, n, y, y, y, y, n,
            y, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n,
            n, n, n, n, n, n, n, n, n, n, n, y, y, y, y, y,
            m, m, m, m, m, m, m, m, m, m, m, m, m, m, m, m,
            m, m, m, m, m, m, m, m, m, m, m, m, m, m, m, m,
            m, m, m, m, m, m, m, m, m, m, m, m, m, m, m, m,
            m, m, m, m, m, m, m, m, m, m, m, m, m, m, m, m,
            m, m, m, m, m, m, m, m, m, m, m, m, m, m, m, m,
            m, m, m, m, m, m, m, m, m, m, m, m, m, m, m, m,
            m, m, m, m, m, m, m, m, m, m, m, m, m, m, m, m,
            m, m, m, m, m, m, m, m, m, m, m, m, m, m, m, m
        ];
        immutable ubyte result = LOOKUP_TABLE[c];
        if (result == n)
            return false;
        if (result == y)
            return true;
        if (result == m)
        {
            auto r = range;
            range.popFrontN(offset);
            return (r.canPeek(2) && (r.peek(2) == "\u2028"
                || r.peek(2) == "\u2029"));
        }
        assert (false);
    }



    enum tokenStart = q{
        size_t index = range.index;
        size_t column = range.column;
        size_t line = range.line;
        auto mark = range.mark();
    };

    void error(string message)
    {
        messages ~= Message(range.line, range.column, message, true);
    }

    void warning(string message)
    {
        messages ~= Message(range.line, range.column, message, false);
        assert (messages.length > 0);
    }

    static struct Message
    {
        size_t line;
        size_t column;
        string message;
        bool isError;
    }

    Message[] messages;
    StringCache* cache;
    LexerConfig config;
    bool haveSSE42;
}

/**
 * Creates a token range from the given source code. Creates a default lexer
 * configuration and a GC-managed string cache.
 */
public auto byToken(R)(R range)
if (is(Unqual!(ElementEncodingType!R) : ubyte) && isDynamicArray!R)
{
    LexerConfig config;
    StringCache* cache = new StringCache(range.length.optimalBucketCount);
    return DLexer(range, config, cache);
}

/**
 * Creates a token range from the given source code. Uses the given string
 * cache.
 */
public auto byToken(R)(R range, StringCache* cache)
if (is(Unqual!(ElementEncodingType!R) : ubyte) && isDynamicArray!R)
{
    LexerConfig config;
    return DLexer(range, config, cache);
}

/**
 * Creates a token range from the given source code. Uses the provided lexer
 * configuration and string cache.
 */
public auto byToken(R)(R range, const LexerConfig config, StringCache* cache)
if (is(Unqual!(ElementEncodingType!R) : ubyte) && isDynamicArray!R)
{
    return DLexer(range, config, cache);
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
    import std.string : lineSplitter, stripRight;

    string leadingChars;
    size_t i = 3;
    size_t j;
    bool hasOutput = false;
    bool lastWasBlank = false;
    switch (comment[0 .. 3])
    {
    case "///":
        j = comment.length;

        foreach (line; lineSplitter(comment))
        {
            auto l = line[3 .. $];
            if (leadingChars.empty)
            {
                size_t k = 0;
                while (k < l.length && (l[k] == ' ' || l[k] == '\t')) k++;
                leadingChars = l[0 .. k];
            }
            immutable string stripped = l.stripRight();
            if (hasOutput)
                outputRange.put('\n');
            else
                hasOutput = true;
            if (stripped.length >= leadingChars.length && stripped.startsWith(leadingChars))
                outputRange.put(stripped[leadingChars.length .. $]);
            else
                outputRange.put(stripped);
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

        while (true)
        {
            if (i == text.length - 1)
            {
                storeLine(text.length);
                break;
            }
            if (text[i] == '\n')
            {
                storeLine(i);
                startIndex = i + 1;
            }
            else if (text[i .. i+2] == "\r\n")
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

/**
 * Helper function used to avoid too much allocations while lexing.
 *
 * Params:
 *      size = The length in bytes of the source file.
 *
 * Returns:
 *      The optimal initial bucket count a `StringCache` should have.
 */
size_t optimalBucketCount(size_t size)
{
    import std.math : nextPow2;
    return nextPow2((size + 31U) / 32U).min(1U << 30U);
}
///
unittest
{
    assert(optimalBucketCount(1) == 2);
    assert(optimalBucketCount(9000 * 32) == 16384);
    assert(optimalBucketCount(100_000_000_000UL) == 1 << 30);
}

/**
 * The string cache is used for string interning.
 *
 * It will only store a single copy of any string that it is asked to hold.
 * Interned strings can be compared for equality by comparing their $(B .ptr)
 * field.
 *
 * Default and postbilt constructors are disabled. When a StringCache goes out
 * of scope, the memory held by it is freed.
 *
 * See_also: $(LINK http://en.wikipedia.org/wiki/String_interning)
 */
struct StringCache
{
public pure nothrow @nogc:

    @disable this();
    @disable this(this);

    /**
     * Params: bucketCount = the initial number of buckets. Must be a
     * power of two
     */
    this(size_t bucketCount) nothrow @trusted @nogc
    in
    {
        import core.bitop : popcnt;
        static if (size_t.sizeof == 8)
        {
            immutable low = popcnt(cast(uint) bucketCount);
            immutable high = popcnt(cast(uint) (bucketCount >> 32));
            assert ((low == 0 && high == 1) || (low == 1 && high == 0));
        }
        else
        {
            static assert (size_t.sizeof == 4);
            assert (popcnt(cast(uint) bucketCount) == 1);
        }
    }
    do
    {
        buckets = (cast(Node**) calloc((Node*).sizeof, bucketCount))[0 .. bucketCount];
    }

    ~this()
    {
        Block* current = rootBlock;
        while (current !is null)
        {
            Block* prev = current;
            current = current.next;
            free(cast(void*) prev);
        }
        foreach (nodePointer; buckets)
        {
            Node* currentNode = nodePointer;
            while (currentNode !is null)
            {
                if (currentNode.mallocated)
                    free(currentNode.str.ptr);
                Node* prev = currentNode;
                currentNode = currentNode.next;
                free(prev);
            }
        }
        rootBlock = null;
        free(buckets.ptr);
        buckets = null;
    }

    /**
     * Caches a string.
     */
    string intern(const(ubyte)[] str) @safe
    {
        if (str is null || str.length == 0)
            return "";
        return _intern(str);
    }

    /**
     * ditto
     */
    string intern(string str) @trusted
    {
        return intern(cast(ubyte[]) str);
    }

    /**
     * The default bucket count for the string cache.
     */
    static enum defaultBucketCount = 4096;

private:

    string _intern(const(ubyte)[] bytes) @trusted
    {
        immutable uint hash = hashBytes(bytes);
        immutable size_t index = hash & (buckets.length - 1);
        Node* s = find(bytes, hash);
        if (s !is null)
            return cast(string) s.str;
        ubyte[] mem = void;
        bool mallocated = bytes.length > BIG_STRING;
        if (mallocated)
            mem = (cast(ubyte*) malloc(bytes.length))[0 .. bytes.length];
        else
            mem = allocate(bytes.length);
        mem[] = bytes[];
        Node* node = cast(Node*) malloc(Node.sizeof);
        node.str = mem;
        node.hash = hash;
        node.next = buckets[index];
        node.mallocated = mallocated;
        buckets[index] = node;
        return cast(string) mem;
    }

    Node* find(const(ubyte)[] bytes, uint hash) @trusted
    {
        import std.algorithm : equal;
        immutable size_t index = hash & (buckets.length - 1);
        Node* node = buckets[index];
        while (node !is null)
        {
            if (node.hash == hash && bytes == cast(ubyte[]) node.str)
                return node;
            node = node.next;
        }
        return node;
    }

    static uint hashBytes(const(ubyte)[] data) pure nothrow @trusted @nogc
    in
    {
        assert (data !is null);
        assert (data.length > 0);
    }
    do
    {
        immutable uint m = 0x5bd1e995;
        immutable int r = 24;
        uint h = cast(uint) data.length;
        while (data.length >= 4)
        {
            uint k = (cast(ubyte) data[3]) << 24
                | (cast(ubyte) data[2]) << 16
                | (cast(ubyte) data[1]) << 8
                | (cast(ubyte) data[0]);
            k *= m;
            k ^= k >> r;
            k *= m;
            h *= m;
            h ^= k;
            data = data[4 .. $];
        }
        switch (data.length & 3)
        {
        case 3:
            h ^= data[2] << 16;
            goto case;
        case 2:
            h ^= data[1] << 8;
            goto case;
        case 1:
            h ^= data[0];
            h *= m;
            break;
        default:
            break;
        }
        h ^= h >> 13;
        h *= m;
        h ^= h >> 15;
        return h;
    }

    ubyte[] allocate(size_t numBytes) pure nothrow @trusted @nogc
    in
    {
        assert (numBytes != 0);
    }
    out (result)
    {
        assert (result.length == numBytes);
    }
    do
    {
        Block* r = rootBlock;
        size_t i = 0;
        while  (i <= 3 && r !is null)
        {
            immutable size_t available = r.bytes.length;
            immutable size_t oldUsed = r.used;
            immutable size_t newUsed = oldUsed + numBytes;
            if (newUsed <= available)
            {
                r.used = newUsed;
                return r.bytes[oldUsed .. newUsed];
            }
            i++;
            r = r.next;
        }
        Block* b = cast(Block*) calloc(Block.sizeof, 1);
        b.used = numBytes;
        b.next = rootBlock;
        rootBlock = b;
        return b.bytes[0 .. numBytes];
    }

    static struct Node
    {
        ubyte[] str = void;
        Node* next = void;
        uint hash = void;
        bool mallocated = void;
    }

    static struct Block
    {
        Block* next;
        size_t used;
        enum BLOCK_CAPACITY = BLOCK_SIZE - size_t.sizeof - (void*).sizeof;
        ubyte[BLOCK_CAPACITY] bytes;
    }

    static assert (BLOCK_SIZE == Block.sizeof);

    enum BLOCK_SIZE = 1024 * 16;

    // If a string would take up more than 1/4 of a block, allocate it outside
    // of the block.
    enum BIG_STRING = BLOCK_SIZE / 4;

    Node*[] buckets;
    Block* rootBlock;
}

private extern(C) void* calloc(size_t, size_t) nothrow pure @nogc @trusted;
private extern(C) void* malloc(size_t) nothrow pure @nogc @trusted;
private extern(C) void free(void*) nothrow pure @nogc @trusted;

unittest
{
    auto source = cast(ubyte[]) q{ import std.stdio;}c;
    auto tokens = getTokensForParser(source, LexerConfig(),
        new StringCache(StringCache.defaultBucketCount));
    assert (tokens.map!"a.type"().equal([tok!"import", tok!"identifier", tok!".",
        tok!"identifier", tok!";"]));
}

/// Test \x char sequence
unittest
{
    auto toks = (string s) => byToken(cast(ubyte[])s);

    // valid
    immutable hex = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','A','B','C','D','E','F'];
    auto source = "";
    foreach (h1; hex)
        foreach (h2; hex)
            source ~= "'\\x" ~ h1 ~ h2 ~ "'";
    assert (toks(source).filter!(t => t.type != tok!"characterLiteral").empty);

    // invalid
    assert (toks(`'\x'`).messages[0] == DLexer.Message(1,4,"Error: 2 hex digits expected.",true));
    assert (toks(`'\x_'`).messages[0] == DLexer.Message(1,4,"Error: 2 hex digits expected.",true));
    assert (toks(`'\xA'`).messages[0] == DLexer.Message(1,5,"Error: 2 hex digits expected.",true));
    assert (toks(`'\xAY'`).messages[0] == DLexer.Message(1,5,"Error: 2 hex digits expected.",true));
    assert (toks(`'\xXX'`).messages[0] == DLexer.Message(1,4,"Error: 2 hex digits expected.",true));
}

version (iasm64NotWindows)
{
    /**
     * Returns:
     */
    ushort newlineMask(const ubyte*) pure nothrow @trusted @nogc
    {
        asm pure nothrow @nogc
        {
            naked;
            movdqu XMM1, [RDI];
            mov RAX, 3;
            mov RDX, 16;
            mov R8, 0x0d0d0d0d0d0d0d0dL;
            movq XMM2, R8;
            shufpd XMM2, XMM2, 0;
            pcmpeqb XMM2, XMM1;
            mov R9, 0x0a0a0a0a0a0a0a0aL;
            movq XMM3, R9;
            shufpd XMM3, XMM3, 0;
            pcmpeqb XMM3, XMM1;
            mov R10, 0xe280a8L;
            movq XMM4, R10;
            pcmpestrm XMM4, XMM1, 0b01001100;
            movdqa XMM4, XMM0;
            mov R11, 0xe280a9L;
            movq XMM5, R11;
            pcmpestrm XMM5, XMM1, 0b01001100;
            movdqa XMM5, XMM0;
            mov RCX, 0x0a0d;
            dec RAX;
            movq XMM6, RCX;
            pcmpestrm XMM6, XMM1, 0b01001100;
            movdqa XMM6, XMM0;
            movdqa XMM7, XMM6;
            pslldq XMM7, 1;
            movdqa XMM0, XMM4;
            por XMM0, XMM5;
            por XMM7, XMM6;
            movdqa XMM1, XMM2;
            por XMM1, XMM3;
            pxor XMM7, XMM1;
            por XMM7, XMM0;
            por XMM7, XMM6;
            pmovmskb RAX, XMM7;
            and RAX, 0b0011_1111_1111_1111;
            ret;
        }
    }

    /**
     * Skips between 0 and 16 bytes that match (or do not match) one of the
     * given $(B chars).
     */
    void skip(bool matching, chars...)(const ubyte*, ulong*, ulong*) pure nothrow
        @trusted @nogc if (chars.length <= 8)
    {
        enum constant = ByteCombine!chars;
        enum charsLength = chars.length;
        static if (matching)
            enum flags = 0b0001_0000;
        else
            enum flags = 0b0000_0000;
        asm pure nothrow @nogc
        {
            naked;
            movdqu XMM1, [RDX];
            mov R10, constant;
            movq XMM2, R10;
            mov RAX, charsLength;
            mov RDX, 16;
            pcmpestri XMM2, XMM1, flags;
            add [RSI], RCX;
            add [RDI], RCX;
            ret;
        }
    }

    /**
     * Returns: the number of bytes starting at the given location that match
     *     (or do not match if $(B invert) is true) the byte ranges in $(B chars).
     */
    ulong rangeMatch(bool invert, chars...)(const ubyte*) pure nothrow @trusted @nogc
    {
        static assert (chars.length % 2 == 0);
        enum constant = ByteCombine!chars;
        static if (invert)
            enum rangeMatchFlags = 0b0000_0100;
        else
            enum rangeMatchFlags = 0b0001_0100;
        enum charsLength = chars.length;
        asm pure nothrow @nogc
        {
            naked;
            movdqu XMM1, [RDI];
            mov R10, constant;
            movq XMM2, R10;
            mov RAX, charsLength;
            mov RDX, 16;
            pcmpestri XMM2, XMM1, rangeMatchFlags;
            mov RAX, RCX;
            ret;
        }
    }

    template ByteCombine(c...)
    {
        static assert (c.length <= 8);
        static if (c.length > 1)
            enum ulong ByteCombine = c[0] | (ByteCombine!(c[1..$]) << 8);
        else
            enum ulong ByteCombine = c[0];
    }
}

unittest
{
    import core.exception : RangeError;
    import std.exception : assertNotThrown;

    static immutable src1 = "/++";
    static immutable src2 = "/**";

    LexerConfig cf;
    StringCache ca = StringCache(16);

    assertNotThrown!RangeError(getTokensForParser(src1, cf, &ca));
    assertNotThrown!RangeError(getTokensForParser(src2, cf, &ca));
}

unittest
{
    static immutable src = `"\eeee"`;

    LexerConfig cf;
    StringCache ca = StringCache(16);

    auto l = DLexer(src, cf, &ca);
    assert(l.front().type == tok!"");
    assert(!l.messages.empty);
}
