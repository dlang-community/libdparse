module dparse.istring;

import dparse.ast;
import dparse.lexer;
import dparse.parser;

import std.array;
import std.string;
import dparse.rollback_allocator;

/// Splits the interpolated string into its parts (not recursive):
/// - `InterpolatedStringLiteralPlain` plain text part / no interpolation
/// - `InterpolatedStringLiteralVariable` a `$identifier` variable
/// - `InterpolatedStringLiteralExpression` a `$(...)` expression
///
/// Params:
///     parserConfig = uses the allocator to create the returned class instances,
///         also used in whole for controlling the parser of embedded expressions.
///     lexerConfig = for embedded expressions, lexer config
///     stringCache = for embedded expressions, string cache
///     istring = the `tok!"istringLiteral"` token containing e.g. `i"hello $name"`
InterpolatedStringLiteralPart[] parseIStringParts(
    ParserConfig parserConfig,
    LexerConfig lexerConfig,
    StringCache* stringCache,
    return Token istring
)
{
    auto tokens = tokenizeIString(istring);
    auto ret = new typeof(return)(tokens.length);

    auto allocator = parserConfig.allocator;

    foreach (i, ref retNode; ret)
    {
        auto token = tokens[i];
        switch (token.type)
        {
        case tok!"stringLiteral":
            auto node = allocator.make!InterpolatedStringLiteralPlain();
            node.sourceText = token.text;
            node.tokens = tokens[i .. i + 1];
            retNode = node;
            break;
        case tok!"identifier":
            auto node = allocator.make!InterpolatedStringLiteralVariable();
            node.identifier = token.text;
            node.tokens = tokens[i .. i + 1];
            retNode = node;
            break;
        case tok!"specialTokenSequence":
            auto node = allocator.make!InterpolatedStringLiteralExpression();
            node.tokens = getTokensForParser(token.text, lexerConfig, stringCache);
            scope parser = new Parser();
            with (parserConfig)
            {
                parser.fileName = fileName;
                parser.tokens = node.tokens;
                parser.messageFunction = messageFunction;
                parser.messageDelegate = messageDelegate;
                parser.allocator = parserConfig.allocator;
            }
            node.expression = parser.parseExpression();
            retNode = node;
            break;
        default:
            assert(false);
        }
        retNode.index = token.index;
    }

    return ret;
}

/// Transforms `i"hello $name $(something + 2)"` into
/// - tok!"stringLiteral"(index:2, text:"hello ")
/// - tok!"identifier"(index:9, text:"name")
/// - tok!"stringLiteral"(index:13, text:" ")
/// - tok!"specialTokenSequence"(index:16, text:"something + 2")
/// all tokens are offset by the input `istring.{index, line, column}`
Token[] tokenizeIString(return Token istring)
in (istring.text.startsWith(`i"`))
in (istring.text.endsWith(`"`))
{
    import std.experimental.lexer : LexerRange;

    enum State
    {
        plain,
        escape,
        dollar,
        identifier,
        expression
    }

    auto indexOffset = istring.index;

    auto bytes = cast(const(ubyte)[]) istring.text[0 .. $ - 1]; // remove trailing `"`
    auto range = LexerRange(bytes, 0, istring.column, istring.line);
    // skip `i"`
    range.popFront();
    range.popFront();

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

    State state;
    int depth;
    auto ret = appender!(Token[]);
    auto startMark = range;

    while (!range.empty)
    {
        char c = cast(char) range.front;
        final switch (state)
        {
        case State.dollar:
            auto dollarIndex = range.index - 1;
            bool skipConsume = false;
            if (c == '(')
            {
                state = State.expression;
                depth = 1;
            }
            else if (DLexer.isIdentifierSeparating(istring.text, range.index)
                || (c >= '0' && c <= '9'))
                goto case State.plain;
            else
            {
                state = State.identifier;
                skipConsume = true;
            }

            if (startMark.index != dollarIndex)
            {
                Token t;
                t.type = tok!"stringLiteral";
                t.text = istring.text[startMark.index .. dollarIndex];
                t.index = startMark.index + indexOffset;
                t.column = startMark.column;
                t.line = startMark.line;
                ret ~= t;
            }

            if (!skipConsume)
                popFrontWhitespaceAware();
            startMark = range;
            continue;
        case State.plain:
            if (c == '\\')
                state = State.escape;
            else if (c == '$')
                state = State.dollar;
            else
                state = State.plain;
            break;
        case State.escape:
            state = State.plain;
            break;
        case State.identifier:
            if (!DLexer.isIdentifierSeparating(istring.text, range.index))
                break;
            Token t;
            t.type = tok!"identifier";
            t.text = istring.text[startMark.index .. range.index];
            t.index = startMark.index + indexOffset;
            t.column = startMark.column;
            t.line = startMark.line;
            startMark = range;
            ret ~= t;
            goto case State.plain;
        case State.expression:
            if (c == '(')
                depth++;
            else if (c == ')')
            {
                depth--;
                if (depth == 0)
                {
                    Token t;
                    t.type = tok!"specialTokenSequence";
                    t.text = istring.text[startMark.index .. range.index];
                    t.index = startMark.index + indexOffset;
                    t.column = startMark.column;
                    t.line = startMark.line;
                    range.popFront();
                    startMark = range;
                    ret ~= t;
                    state = State.plain;
                    continue;
                }
            }
            break;
        }

        popFrontWhitespaceAware();
    }

    final switch (state)
    {
    case State.dollar:
    case State.plain:
    case State.escape:
        if (startMark.index == bytes.length)
            break;
        Token t;
        t.type = tok!"stringLiteral";
        t.text = istring.text[startMark.index .. bytes.length];
        t.index = startMark.index + indexOffset;
        t.column = startMark.column;
        t.line = startMark.line;
        ret ~= t;
        break;
    case State.identifier:
        Token t;
        t.type = tok!"identifier";
        t.text = istring.text[startMark.index .. bytes.length];
        t.index = startMark.index + indexOffset;
        t.column = startMark.column;
        t.line = startMark.line;
        ret ~= t;
        break;
    case State.expression:
        Token t;
        t.type = tok!"specialTokenSequence";
        t.text = istring.text[startMark.index .. bytes.length];
        t.index = startMark.index + indexOffset;
        t.column = startMark.column;
        t.line = startMark.line;
        ret ~= t;
        break;
    }

    return ret.data;
}

///
unittest
{
    import std.conv;

    Token input;
    input.text = `i"hello $name $(something + 2)"`;
    input.line = 5;
    input.column = 10;
    input.index = 100;
    auto tokens = tokenizeIString(input);

    auto all = "tokens:\n" ~ tokens.to!(string[]).join("\n");

    assert(tokens.length == 4, all);

    assert(tokens[0].type == tok!"stringLiteral", all);
    assert(tokens[0].index == 102, all);
    assert(tokens[0].column == 12, all);
    assert(tokens[0].line == 5, all);
    assert(tokens[0].text == "hello ", all);

    assert(tokens[1].type == tok!"identifier", all);
    assert(tokens[1].index == 109, all);
    assert(tokens[1].column == 19, all);
    assert(tokens[1].line == 5, all);
    assert(tokens[1].text == "name", all);

    assert(tokens[2].type == tok!"stringLiteral", all);
    assert(tokens[2].index == 113, all);
    assert(tokens[2].column == 23, all);
    assert(tokens[2].line == 5, all);
    assert(tokens[2].text == " ", all);

    assert(tokens[3].type == tok!"specialTokenSequence", all);
    assert(tokens[3].index == 116, all);
    assert(tokens[3].column == 26, all);
    assert(tokens[3].line == 5, all);
    assert(tokens[3].text == "something + 2", all);
}

unittest
{
    auto test(string content)
    {
        Token input;
        input.text = content;
        input.line = 1;
        input.column = 1;
        input.index = 0;
        auto tokens = tokenizeIString(input);
        char[] ret = new char[content.length];
        ret[] = ' ';
        foreach (t; tokens)
        {
            assert(t.text.length);
            ret[t.index .. t.index + t.text.length] =
                t.type == tok!"identifier" ? 'i' :
                t.type == tok!"stringLiteral" ? '.' :
                t.type == tok!"specialTokenSequence" ? '*' : '?';
        }
        return ret;
    }

    // dfmt off

    assert(test(`i"$name"`)
             == `   iiii `);

    assert(test(`i"plain"`)
             == `  ..... `);

    assert(test(`i"$(expression)"`)
             == `    **********  `);

    assert(test(`i"$(expression"`)
             == `    ********** `);

    assert(test(`i"$name "`)
             == `   iiii. `);

    assert(test(`i"$ plain"`)
             == `  ....... `);

    assert(test(`i"$0 plain"`)
             == `  ........ `);

    assert(test(`i"$$0 plain"`)
             == `  ......... `);

    assert(test(`i"$.1 plain"`)
             == `  ......... `);

    assert(test(`i"I have $$money"`)
             == `  ........ iiiii `);

    // dfmt on
}
