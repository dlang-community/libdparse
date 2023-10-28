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
///     istringText = the token text of an istring such as `i"hello $name"`
InterpolatedStringLiteralPart[] parseIStringParts(
    ParserConfig parserConfig,
    LexerConfig lexerConfig,
    StringCache* stringCache,
    return string istringText
)
{
    auto tokens = tokenizeIString(istringText);
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
Token[] tokenizeIString(return string istringText)
in (istringText.startsWith(`i"`))
in (istringText.endsWith(`"`))
{
    enum State
    {
        plain,
        escape,
        dollar,
        identifier,
        expression
    }

    istringText = istringText[0 .. $ - 1]; // remove trailing `"`

    State state;
    int depth;
    auto ret = appender!(Token[]);

    // i = 2, to skip the starting `i"`
    size_t lastEnd = 2;
    for (size_t i = lastEnd; i < istringText.length; i++)
    {
        char c = istringText[i];
        final switch (state)
        {
        case State.dollar:
            auto dollarIndex = i - 1;
            if (c == '(')
            {
                state = State.expression;
                depth = 1;
            }
            else if (DLexer.isIdentifierSeparating(istringText, i)
                || (c >= '0' && c <= '9'))
                goto case State.plain;
            else
            {
                state = State.identifier;
                i--;
            }

            if (lastEnd != dollarIndex)
            {
                Token t;
                t.type = tok!"stringLiteral";
                t.text = istringText[lastEnd .. dollarIndex];
                t.index = t.column = lastEnd;
                ret ~= t;
            }
            lastEnd = i + 1;
            break;
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
            if (!DLexer.isIdentifierSeparating(istringText, i))
                break;
            Token t;
            t.type = tok!"identifier";
            t.text = istringText[lastEnd .. i];
            t.index = t.column = lastEnd;
            lastEnd = i;
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
                    t.text = istringText[lastEnd .. i];
                    t.index = t.column = lastEnd;
                    lastEnd = i + 1;
                    ret ~= t;
                    state = State.plain;
                }
            }
            break;
        }
    }

    final switch (state)
    {
    case State.dollar:
    case State.plain:
    case State.escape:
        if (lastEnd == istringText.length)
            break;
        Token t;
        t.type = tok!"stringLiteral";
        t.text = istringText[lastEnd .. $];
        t.index = t.column = lastEnd;
        ret ~= t;
        break;
    case State.identifier:
        Token t;
        t.type = tok!"identifier";
        t.text = istringText[lastEnd .. $];
        t.index = t.column = lastEnd;
        ret ~= t;
        break;
    case State.expression:
        Token t;
        t.type = tok!"specialTokenSequence";
        t.text = istringText[lastEnd .. $];
        t.index = t.column = lastEnd;
        ret ~= t;
        break;
    }

    return ret.data;
}

///
unittest
{
    auto tokens = tokenizeIString(`i"hello $name $(something + 2)"`);

    assert(tokens.length == 4);

    assert(tokens[0].type == tok!"stringLiteral");
    assert(tokens[0].index == 2);
    assert(tokens[0].text == "hello ");

    assert(tokens[1].type == tok!"identifier");
    assert(tokens[1].index == 9);
    assert(tokens[1].text == "name");

    assert(tokens[2].type == tok!"stringLiteral");
    assert(tokens[2].index == 13);
    assert(tokens[2].text == " ");

    assert(tokens[3].type == tok!"specialTokenSequence");
    assert(tokens[3].index == 16);
    assert(tokens[3].text == "something + 2");
}

unittest
{
    auto tokens = tokenizeIString(`i"$name"`);
    assert(tokens.length == 1);
    assert(tokens[0].type == tok!"identifier");
    assert(tokens[0].index == 3);
    assert(tokens[0].text == "name");

    tokens = tokenizeIString(`i"plain"`);
    assert(tokens.length == 1);
    assert(tokens[0].type == tok!"stringLiteral");
    assert(tokens[0].index == 2);
    assert(tokens[0].text == "plain");

    tokens = tokenizeIString(`i"$(expression)"`);
    assert(tokens.length == 1);
    assert(tokens[0].type == tok!"specialTokenSequence");
    assert(tokens[0].index == 4);
    assert(tokens[0].text == "expression");

    tokens = tokenizeIString(`i"$(expression"`);
    assert(tokens.length == 1);
    assert(tokens[0].type == tok!"specialTokenSequence");
    assert(tokens[0].index == 4);
    assert(tokens[0].text == "expression");

    tokens = tokenizeIString(`i"$name "`);
    assert(tokens.length == 2);
    assert(tokens[0].type == tok!"identifier");
    assert(tokens[0].index == 3);
    assert(tokens[0].text == "name");
    assert(tokens[1].type == tok!"stringLiteral");
    assert(tokens[1].index == 7);
    assert(tokens[1].text == " ");

    tokens = tokenizeIString(`i"$ plain"`);
    assert(tokens.length == 1);
    assert(tokens[0].type == tok!"stringLiteral");
    assert(tokens[0].index == 2);
    assert(tokens[0].text == "$ plain");

    tokens = tokenizeIString(`i"$0 plain"`);
    assert(tokens.length == 1);
    assert(tokens[0].type == tok!"stringLiteral");
    assert(tokens[0].index == 2);
    assert(tokens[0].text == "$0 plain");

    tokens = tokenizeIString(`i"$$0 plain"`);
    assert(tokens.length == 1);
    assert(tokens[0].type == tok!"stringLiteral");
    assert(tokens[0].index == 2);
    assert(tokens[0].text == "$$0 plain");

    tokens = tokenizeIString(`i"$.1 plain"`);
    assert(tokens.length == 1);
    assert(tokens[0].type == tok!"stringLiteral");
    assert(tokens[0].index == 2);
    assert(tokens[0].text == "$.1 plain");

    tokens = tokenizeIString(`i"I have $$money"`);
    assert(tokens.length == 2);
    assert(tokens[0].type == tok!"stringLiteral");
    assert(tokens[0].index == 2);
    assert(tokens[0].text == "I have $");

    assert(tokens[1].type == tok!"identifier");
    assert(tokens[1].index == 11);
    assert(tokens[1].text == "money");
}
