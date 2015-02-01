// Written in the D programming language

module std.d.parser;

import std.d.lexer;
import std.d.ast;
import std.allocator;
import std.conv;
import std.algorithm;
import std.array;
import std.stdio;
import std.string : format;

// Uncomment this if you want ALL THE OUTPUT
// Caution: generates 180 megabytes of logging for std.datetime
//version = std_parser_verbose;

alias ParseAllocator = CAllocatorImpl!(Mallocator);

/**
 * Params:
 *     tokens = the tokens parsed by std.d.lexer
 *     fileName = the name of the file being parsed
 *     messageFunction = a function to call on error or warning messages.
 *         The parameters are the file name, line number, column number,
 *         the error or warning message, and a boolean (true means error, false
 *         means warning).
 * Returns: the parsed module
 */
Module parseModule(const(Token)[] tokens, string fileName, CAllocator allocator = null,
    void function(string, size_t, size_t, string, bool) messageFunction = null,
    uint* errorCount = null, uint* warningCount = null)
{
    auto parser = new Parser();
    parser.fileName = fileName;
    parser.tokens = tokens;
    parser.messageFunction = messageFunction;
    parser.allocator = allocator;
    auto mod = parser.parseModule();
    if (warningCount !is null)
        *warningCount = parser.warningCount;
    if (errorCount !is null)
        *errorCount = parser.errorCount;
    return mod;
}

/**
 * D Parser.
 *
 * It is sometimes useful to sub-class Parser to skip over things that are not
 * interesting. For example, DCD skips over function bodies when caching symbols
 * from imported files.
 */
class Parser
{
    /**
     * Parses an AddExpression.
     *
     * $(GRAMMAR $(RULEDEF addExpression):
     *       $(RULE mulExpression)
     *     | $(RULE addExpression) $(LPAREN)$(LITERAL '+') | $(LITERAL'-') | $(LITERAL'~')$(RPAREN) $(RULE mulExpression)
     *     ;)
     */
    ExpressionNode parseAddExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AddExpression, MulExpression,
            tok!"+", tok!"-", tok!"~")();
    }

    /**
     * Parses an AliasDeclaration.
     *
     * $(GRAMMAR $(RULEDEF aliasDeclaration):
     *       $(LITERAL 'alias') $(RULE aliasInitializer) $(LPAREN)$(LITERAL ',') $(RULE aliasInitializer)$(RPAREN)* $(LITERAL ';')
     *     | $(LITERAL 'alias') $(RULE storageClass)* $(RULE type) $(LITERAL identifierList) $(LITERAL ';')
     *     ;)
     */
    AliasDeclaration parseAliasDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!AliasDeclaration;
        mixin (nullCheck!`expect(tok!"alias")`);
        node.comment = comment;
        comment = null;

        if (startsWith(tok!"identifier", tok!"=") || startsWith(tok!"identifier", tok!"("))
        {
            AliasInitializer[] initializers;
            do
            {
                auto initializer = parseAliasInitializer();
                mixin (nullCheck!`initializer`);
                initializers ~= initializer;
                if (currentIs(tok!","))
                    advance();
                else
                    break;
            }
            while (moreTokens());
            node.initializers = ownArray(initializers);
        }
        else
        {
            StorageClass[] storageClasses;
            while (moreTokens() && isStorageClass())
            storageClasses ~= parseStorageClass();
            if (storageClasses.length > 0)
                node.storageClasses = ownArray(storageClasses);
            warn("Prefer the new \"'alias' identifier '=' type ';'\" syntax"
                ~ " to the  old \"'alias' type identifier ';'\" syntax");
            mixin (nullCheck!`node.type = parseType()`);
            mixin (nullCheck!`node.identifierList = parseIdentifierList()`);
        }
        mixin (nullCheck!`expect(tok!";")`);
        return node;
    }

    /**
     * Parses an AliasInitializer.
     *
     * $(GRAMMAR $(RULEDEF aliasInitializer):
     *     $(LITERAL Identifier) $(RULE templateParameters)? $(LITERAL '=') $(RULE storageClass)* $(RULE type)
     *     ;)
     */
    AliasInitializer parseAliasInitializer()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!AliasInitializer;
        auto i = expect(tok!"identifier");
        mixin (nullCheck!`i`);
        node.name = *i;
        if (currentIs(tok!"("))
            mixin (nullCheck!`node.templateParameters = parseTemplateParameters()`);
        mixin (nullCheck!`expect(tok!"=")`);
        StorageClass[] storageClasses;
        while (moreTokens() && isStorageClass())
            storageClasses ~= parseStorageClass();
        if (storageClasses.length > 0)
            node.storageClasses = ownArray(storageClasses);
        mixin (nullCheck!`node.type = parseType()`);
        return node;
    }

    /**
     * Parses an AliasThisDeclaration.
     *
     * $(GRAMMAR $(RULEDEF aliasThisDeclaration):
     *     $(LITERAL 'alias') $(LITERAL Identifier) $(LITERAL 'this') $(LITERAL ';')
     *     ;)
     */
    AliasThisDeclaration parseAliasThisDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!AliasThisDeclaration;
        mixin (nullCheck!`expect(tok!"alias")`);
        auto ident = expect(tok!"identifier");
        mixin (nullCheck!`ident`);
        node.identifier = *ident;
        mixin (nullCheck!`expect(tok!"this")`);
        mixin (nullCheck!`expect(tok!";")`);
        return node;
    }

    /**
     * Parses an AlignAttribute.
     *
     * $(GRAMMAR $(RULEDEF alignAttribute):
     *     $(LITERAL 'align') ($(LITERAL '$(LPAREN)') $(LITERAL IntegerLiteral) $(LITERAL '$(RPAREN)'))?
     *     ;)
     */
    AlignAttribute parseAlignAttribute()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!AlignAttribute;
        expect(tok!"align");
        if (currentIs(tok!"("))
        {
            mixin (nullCheck!`expect(tok!"(")`);
            auto intLit = expect(tok!"intLiteral");
            mixin (nullCheck!`intLit`);
            node.intLiteral = *intLit;
            mixin (nullCheck!`expect(tok!")")`);
        }
        return node;
    }

    /**
     * Parses an AndAndExpression.
     *
     * $(GRAMMAR $(RULEDEF andAndExpression):
     *       $(RULE orExpression)
     *     | $(RULE andAndExpression) $(LITERAL '&&') $(RULE orExpression)
     *     ;)
     */
    ExpressionNode parseAndAndExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AndAndExpression, OrExpression,
            tok!"&&")();
    }

    /**
     * Parses an AndExpression.
     *
     * $(GRAMMAR $(RULEDEF andExpression):
     *       $(RULE cmpExpression)
     *     | $(RULE andExpression) $(LITERAL '&') $(RULE cmpExpression)
     *     ;)
     */
    ExpressionNode parseAndExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AndExpression, CmpExpression,
            tok!"&")();
    }

    /**
     * Parses an ArgumentList.
     *
     * $(GRAMMAR $(RULEDEF argumentList):
     *     $(RULE assignExpression) ($(LITERAL ',') $(RULE assignExpression)?)*
     *     ;)
     */
    ArgumentList parseArgumentList()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        if (!moreTokens)
        {
            error("argument list expected instead of EOF");
            return null;
        }
        size_t startLocation = current().index;
        auto node = parseCommaSeparatedRule!(ArgumentList, AssignExpression)(true);
        mixin (nullCheck!`node`);
        node.startLocation = startLocation;
        if (moreTokens) node.endLocation = current().index;
        return node;
    }

    /**
     * Parses Arguments.
     *
     * $(GRAMMAR $(RULEDEF arguments):
     *     $(LITERAL '$(LPAREN)') $(RULE argumentList)? $(LITERAL '$(RPAREN)')
     *     ;)
     */
    Arguments parseArguments()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Arguments;
        mixin (nullCheck!`expect(tok!"(")`);
        if (!currentIs(tok!")"))
            mixin (nullCheck!`node.argumentList = parseArgumentList()`);
        mixin (nullCheck!`expect(tok!")")`);
        return node;
    }

    /**
     * Parses an ArrayInitializer.
     *
     * $(GRAMMAR $(RULEDEF arrayInitializer):
     *       $(LITERAL '[') $(LITERAL ']')
     *     | $(LITERAL '[') $(RULE arrayMemberInitialization) ($(LITERAL ',') $(RULE arrayMemberInitialization)?)* $(LITERAL ']')
     *     ;)
     */
    ArrayInitializer parseArrayInitializer()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!ArrayInitializer;
        mixin (nullCheck!`expect(tok!"[")`);
        ArrayMemberInitialization[] arrayMemberInitializations;
        while (moreTokens())
        {
            if (currentIs(tok!"]"))
                break;
            arrayMemberInitializations ~= parseArrayMemberInitialization();
            if (currentIs(tok!","))
                advance();
            else
                break;
        }
        node.arrayMemberInitializations = ownArray(arrayMemberInitializations);
        mixin (nullCheck!`expect(tok!"]")`);
        return node;
    }

    /**
     * Parses an ArrayLiteral.
     *
     * $(GRAMMAR $(RULEDEF arrayLiteral):
     *     $(LITERAL '[') $(RULE argumentList)? $(LITERAL ']')
     *     ;)
     */
    ArrayLiteral parseArrayLiteral()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!ArrayLiteral;
        mixin (nullCheck!`expect(tok!"[")`);
        if (!currentIs(tok!"]"))
            mixin (nullCheck!`node.argumentList = parseArgumentList()`);
        mixin (nullCheck!`expect(tok!"]")`);
        return node;
    }

    /**
     * Parses an ArrayMemberInitialization.
     *
     * $(GRAMMAR $(RULEDEF arrayMemberInitialization):
     *     ($(RULE assignExpression) $(LITERAL ':'))? $(RULE nonVoidInitializer)
     *     ;)
     */
    ArrayMemberInitialization parseArrayMemberInitialization()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!ArrayMemberInitialization;
        switch (current.type)
        {
        case tok!"[":
            auto b = setBookmark();
            skipBrackets();
            if (currentIs(tok!":"))
            {
                mixin (nullCheck!`node.assignExpression = parseAssignExpression()`);
                advance(); // :
                mixin (nullCheck!`node.nonVoidInitializer`);
                break;
            }
            else
            {
                goToBookmark(b);
                goto case;
            }
        case tok!"{":
            mixin (nullCheck!`node.nonVoidInitializer = parseNonVoidInitializer()`);
            break;
        default:
            auto assignExpression = parseAssignExpression();
            mixin (nullCheck!`assignExpression`);
            if (currentIs(tok!":"))
            {
                node.assignExpression = assignExpression;
                advance();
                mixin (nullCheck!`node.nonVoidInitializer = parseNonVoidInitializer()`);
            }
            else
            {
                node.nonVoidInitializer = allocate!NonVoidInitializer;
                node.nonVoidInitializer.assignExpression = assignExpression;
            }
        }
        return node;
    }

    /**
     * Parses an AsmAddExp
     *
     * $(GRAMMAR $(RULEDEF asmAddExp):
     *       $(RULE asmMulExp)
     *     | $(RULE asmAddExp) ($(LITERAL '+') | $(LITERAL '-')) $(RULE asmMulExp)
     *     ;)
     */
    ExpressionNode parseAsmAddExp()
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AsmAddExp, AsmMulExp,
            tok!"+", tok!"-")();
    }

    /**
     * Parses an AsmAndExp
     *
     * $(GRAMMAR $(RULEDEF asmAndExp):
     *       $(RULE asmEqualExp)
     *     | $(RULE asmAndExp) $(LITERAL '&') $(RULE asmEqualExp)
     *     ;)
     */
    ExpressionNode parseAsmAndExp()
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AsmAndExp, AsmEqualExp, tok!"&");
    }

    /**
     * Parses an AsmBrExp
     *
     * $(GRAMMAR $(RULEDEF asmBrExp):
     *       $(RULE asmUnaExp)
     *     | $(RULE asmBrExp)? $(LITERAL '[') $(RULE asmExp) $(LITERAL ']')
     *     ;)
     */
    AsmBrExp parseAsmBrExp()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        AsmBrExp node = allocate!AsmBrExp();
        size_t line = current.line;
        size_t column = current.column;
        if (currentIs(tok!"["))
        {
            advance(); // [
            mixin (nullCheck!`node.asmExp = parseAsmExp()`);
            mixin (nullCheck!`expect(tok!"]")`);
            if (currentIs(tok!"["))
                goto brLoop;
        }
        else
        {
            mixin (nullCheck!`node.asmUnaExp = parseAsmUnaExp()`);
            brLoop: while (currentIs(tok!"["))
            {
                AsmBrExp br = allocate!AsmBrExp(); // huehuehuehue
                br.asmBrExp = node;
                br.line = current().line;
                br.column = current().column;
                node = br;
                node.line = line;
                node.column = column;
                advance(); // [
                mixin (nullCheck!`node.asmExp = parseAsmExp()`);
                mixin (nullCheck!`expect(tok!"]")`);
            }
        }
        return node;
    }

    /**
     * Parses an AsmEqualExp
     *
     * $(GRAMMAR $(RULEDEF asmEqualExp):
     *       $(RULE asmRelExp)
     *     | $(RULE asmEqualExp) ('==' | '!=') $(RULE asmRelExp)
     *     ;)
     */
    ExpressionNode parseAsmEqualExp()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AsmEqualExp, AsmRelExp, tok!"==", tok!"!=")();
    }

    /**
     * Parses an AsmExp
     *
     * $(GRAMMAR $(RULEDEF asmExp):
     *     $(RULE asmLogOrExp) ($(LITERAL '?') $(RULE asmExp) $(LITERAL ':') $(RULE asmExp))?
     *     ;)
     */
    ExpressionNode parseAsmExp()
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        AsmExp node = allocate!AsmExp;
        mixin (nullCheck!`node.left = parseAsmLogOrExp()`);
        if (currentIs(tok!"?"))
        {
            advance();
            mixin (nullCheck!`(node.middle = parseAsmExp())`);
            mixin (nullCheck!`expect(tok!":")`);
            mixin (nullCheck!`(node.right = parseAsmExp())`);
        }
        return node;
    }

    /**
     * Parses an AsmInstruction
     *
     * $(GRAMMAR $(RULEDEF asmInstruction):
     *       $(LITERAL Identifier)
     *     | $(LITERAL 'align') $(LITERAL IntegerLiteral)
     *     | $(LITERAL 'align') $(LITERAL Identifier)
     *     | $(LITERAL Identifier) $(LITERAL ':') $(RULE asmInstruction)
     *     | $(LITERAL Identifier) $(RULE operands)
     *     | $(LITERAL 'in') $(RULE operands)
     *     | $(LITERAL 'out') $(RULE operands)
     *     | $(LITERAL 'int') $(RULE operands)
     *     ;)
     */
    AsmInstruction parseAsmInstruction()
    {
        import std.range : assumeSorted;
        mixin (traceEnterAndExit!(__FUNCTION__));
        AsmInstruction node = allocate!AsmInstruction;
        if (currentIs(tok!"align"))
        {
            advance(); // align
            node.hasAlign = true;
            if (currentIsOneOf(tok!"intLiteral", tok!"identifier"))
                node.identifierOrIntegerOrOpcode = advance();
            else
                error("Identifier or integer literal expected.");
        }
        else if (currentIsOneOf(tok!"identifier", tok!"in", tok!"out", tok!"int"))
        {
            node.identifierOrIntegerOrOpcode = advance();
            if (node.identifierOrIntegerOrOpcode == tok!"identifier" && currentIs(tok!":"))
            {
                advance(); // :
                mixin (nullCheck!`node.asmInstruction = parseAsmInstruction()`);
            }
            else if (!currentIs(tok!";"))
                mixin (nullCheck!`node.operands = parseOperands()`);
        }
        return node;
    }

    /**
     * Parses an AsmLogAndExp
     *
     * $(GRAMMAR $(RULEDEF asmLogAndExp):
     *     $(RULE asmOrExp)
     *     $(RULE asmLogAndExp) $(LITERAL '&&') $(RULE asmOrExp)
     *     ;)
     */
    ExpressionNode parseAsmLogAndExp()
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AsmLogAndExp, AsmOrExp, tok!"&&");
    }

    /**
     * Parses an AsmLogOrExp
     *
     * $(GRAMMAR $(RULEDEF asmLogOrExp):
     *       $(RULE asmLogAndExp)
     *     | $(RULE asmLogOrExp) '||' $(RULE asmLogAndExp)
     *     ;)
     */
    ExpressionNode parseAsmLogOrExp()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AsmLogOrExp, AsmLogAndExp, tok!"||")();
    }

    /**
     * Parses an AsmMulExp
     *
     * $(GRAMMAR $(RULEDEF asmMulExp):
     *       $(RULE asmBrExp)
     *     | $(RULE asmMulExp) ($(LITERAL '*') | $(LITERAL '/') | $(LITERAL '%')) $(RULE asmBrExp)
     *     ;)
     */
    ExpressionNode parseAsmMulExp()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AsmMulExp, AsmBrExp, tok!"*", tok!"/", tok!"%")();
    }

    /**
     * Parses an AsmOrExp
     *
     * $(GRAMMAR $(RULEDEF asmOrExp):
     *       $(RULE asmXorExp)
     *     | $(RULE asmOrExp) $(LITERAL '|') $(RULE asmXorExp)
     *     ;)
     */
    ExpressionNode parseAsmOrExp()
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AsmOrExp, AsmXorExp, tok!"|")();
    }

    /**
     * Parses an AsmPrimaryExp
     *
     * $(GRAMMAR $(RULEDEF asmPrimaryExp):
     *       $(LITERAL IntegerLiteral)
     *     | $(LITERAL FloatLiteral)
     *     | $(LITERAL StringLiteral)
     *     | $(RULE register)
     *     | $(RULE identifierChain)
     *     | $(LITERAL '$')
     *     ;)
     */
    AsmPrimaryExp parseAsmPrimaryExp()
    {
        import std.range : assumeSorted;
        mixin (traceEnterAndExit!(__FUNCTION__));
        AsmPrimaryExp node = allocate!AsmPrimaryExp();
        switch (current().type)
        {
        case tok!"doubleLiteral":
        case tok!"floatLiteral":
        case tok!"intLiteral":
        case tok!"longLiteral":
        case tok!"stringLiteral":
        case tok!"$":
            node.token = advance();
            break;
        case tok!"identifier":
            if (assumeSorted(REGISTER_NAMES).equalRange(current().text).length > 0)
            {
                trace("Found register");
                mixin (nullCheck!`(node.register = parseRegister())`);
            }
            else
                mixin (nullCheck!`node.identifierChain = parseIdentifierChain()`);
            break;
        default:
            error("Float literal, integer literal, $, or identifier expected.");
            return null;
        }
        return node;
    }

    /**
     * Parses an AsmRelExp
     *
     * $(GRAMMAR $(RULEDEF asmRelExp):
     *       $(RULE asmShiftExp)
     *     | $(RULE asmRelExp) (($(LITERAL '<') | $(LITERAL '<=') | $(LITERAL '>') | $(LITERAL '>=')) $(RULE asmShiftExp))?
     *     ;)
     */
    ExpressionNode parseAsmRelExp()
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AsmRelExp, AsmShiftExp, tok!"<",
            tok!"<=", tok!">", tok!">=")();
    }

    /**
     * Parses an AsmShiftExp
     *
     * $(GRAMMAR $(RULEDEF asmShiftExp):
     *     $(RULE asmAddExp)
     *     $(RULE asmShiftExp) ($(LITERAL '<<') | $(LITERAL '>>') | $(LITERAL '>>>')) $(RULE asmAddExp)
     *     ;)
     */
    ExpressionNode parseAsmShiftExp()
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AsmShiftExp, AsmAddExp, tok!"<<",
            tok!">>", tok!">>>");
    }

    /**
     * Parses an AsmStatement
     *
     * $(GRAMMAR $(RULEDEF asmStatement):
     *     $(LITERAL 'asm') $(RULE functionAttributes)? $(LITERAL '{') $(RULE asmInstruction)+ $(LITERAL '}')
     *     ;)
     */
    AsmStatement parseAsmStatement()
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        AsmStatement node = allocate!AsmStatement;
        AsmInstruction[] instructions;
        advance(); // asm
        FunctionAttribute[] functionAttributes;
        while (isAttribute())
        {
            auto attr = parseFunctionAttribute();
            if (attr is null)
            {
                error("Function attribute or '{' expected");
                return null;
            }
            functionAttributes ~= attr;
        }
        node.functionAttributes = ownArray(functionAttributes);
        advance(); // {
        while (moreTokens() && !currentIs(tok!"}"))
        {
            AsmInstruction instruction = parseAsmInstruction();
            if (instruction is null)
                return null;
            if (!expect(tok!";"))
                return null;
            instructions ~= instruction;
        }
        node.asmInstructions = ownArray(instructions);
        expect(tok!"}");
        return node;
    }

    /**
     * Parses an AsmTypePrefix
     *
     * Note that in the following grammar definition the first identifier must
     * be "near", "far", "word", "dword", or "qword". The second identifier must
     * be "ptr".
     *
     * $(GRAMMAR $(RULEDEF asmTypePrefix):
     *       $(LITERAL Identifier) $(LITERAL Identifier)?
     *     | $(LITERAL 'byte') $(LITERAL Identifier)?
     *     | $(LITERAL 'short') $(LITERAL Identifier)?
     *     | $(LITERAL 'int') $(LITERAL Identifier)?
     *     | $(LITERAL 'float') $(LITERAL Identifier)?
     *     | $(LITERAL 'double') $(LITERAL Identifier)?
     *     | $(LITERAL 'real') $(LITERAL Identifier)?
     *     ;)
     */
    AsmTypePrefix parseAsmTypePrefix()
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        switch (current().type)
        {
        case tok!"identifier":
        case tok!"byte":
        case tok!"short":
        case tok!"int":
        case tok!"float":
        case tok!"double":
        case tok!"real":
            AsmTypePrefix prefix = allocate!AsmTypePrefix();
            prefix.left = advance();
            if (prefix.left.type == tok!"identifier") switch (prefix.left.text)
            {
            case "near":
            case "far":
            case "word":
            case "dword":
            case "qword":
                break;
            default:
                error("ASM type prefix expected");
                return null;
            }
            if (currentIs(tok!"identifier") && current().text == "ptr")
                prefix.right = advance();
            return prefix;
        default:
            error("Expected an identifier, 'byte', 'short', 'int', 'float', 'double', or 'real'");
            return null;
        }
    }

    /**
     * Parses an AsmUnaExp
     *
     * $(GRAMMAR $(RULEDEF asmUnaExp):
     *       $(RULE asmTypePrefix) $(RULE asmExp)
     *     | $(LITERAL Identifier) $(RULE asmExp)
     *     | $(LITERAL '+') $(RULE asmUnaExp)
     *     | $(LITERAL '-') $(RULE asmUnaExp)
     *     | $(LITERAL '!') $(RULE asmUnaExp)
     *     | $(LITERAL '~') $(RULE asmUnaExp)
     *     | $(RULE asmPrimaryExp)
     *     ;)
     */
    AsmUnaExp parseAsmUnaExp()
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        AsmUnaExp node = allocate!AsmUnaExp();
        switch (current().type)
        {
        case tok!"+":
        case tok!"-":
        case tok!"!":
        case tok!"~":
            node.prefix = advance();
            mixin (nullCheck!`node.asmUnaExp = parseAsmUnaExp()`);
            break;
        case tok!"byte":
        case tok!"short":
        case tok!"int":
        case tok!"float":
        case tok!"double":
        case tok!"real":
        typePrefix:
            mixin (nullCheck!`node.asmTypePrefix = parseAsmTypePrefix()`);
            mixin (nullCheck!`node.asmExp = parseAsmExp()`);
            break;
        case tok!"identifier":
            switch (current().text)
            {
            case "offsetof":
            case "seg":
                node.prefix = advance();
                mixin (nullCheck!`node.asmExp = parseAsmExp()`);
                break;
            case "near":
            case "far":
            case "word":
            case "dword":
            case "qword":
                goto typePrefix;
            default:
                goto outerDefault;
            }
            break;
        outerDefault:
        default:
            mixin (nullCheck!`node.asmPrimaryExp = parseAsmPrimaryExp()`);
            break;
        }
        return node;
    }

    /**
     * Parses an AsmXorExp
     *
     * $(GRAMMAR $(RULEDEF asmXorExp):
     *       $(RULE asmAndExp)
     *     | $(RULE asmXorExp) $(LITERAL '^') $(RULE asmAndExp)
     *     ;)
     */
    ExpressionNode parseAsmXorExp()
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(AsmXorExp, AsmAndExp, tok!"^")();
    }

    /**
     * Parses an AssertExpression
     *
     * $(GRAMMAR $(RULEDEF assertExpression):
     *     $(LITERAL 'assert') $(LITERAL '$(LPAREN)') $(RULE assignExpression) ($(LITERAL ',') $(RULE assignExpression))? $(LITERAL '$(RPAREN)')
     *     ;)
     */
    AssertExpression parseAssertExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!AssertExpression;
        expect(tok!"assert");
        mixin (nullCheck!`expect(tok!"(")`);
        mixin (nullCheck!`node.assertion = parseAssignExpression()`);
        if (currentIs(tok!","))
        {
            advance();
            mixin (nullCheck!`node.message = parseAssignExpression()`);
        }
        mixin (nullCheck!`expect(tok!")")`);
        return node;
    }

    /**
     * Parses an AssignExpression
     *
     * $(GRAMMAR $(RULEDEF assignExpression):
     *     $(RULE ternaryExpression) ($(RULE assignOperator) $(RULE assignExpression))?
     *     ;
     *$(RULEDEF assignOperator):
     *       $(LITERAL '=')
     *     | $(LITERAL '>>>=')
     *     | $(LITERAL '>>=')
     *     | $(LITERAL '<<=')
     *     | $(LITERAL '+=')
     *     | $(LITERAL '-=')
     *     | $(LITERAL '*=')
     *     | $(LITERAL '%=')
     *     | $(LITERAL '&=')
     *     | $(LITERAL '/=')
     *     | $(LITERAL '|=')
     *     | $(LITERAL '^^=')
     *     | $(LITERAL '^=')
     *     | $(LITERAL '~=')
     *     ;)
     */
    ExpressionNode parseAssignExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        if (!moreTokens)
        {
            error("Assign expression expected instead of EOF");
            return null;
        }
        auto ternary = parseTernaryExpression();
        if (ternary is null)
            return null;
        if (currentIsOneOf(tok!"=", tok!">>>=",
            tok!">>=", tok!"<<=",
            tok!"+=", tok!"-=", tok!"*=",
            tok!"%=", tok!"&=", tok!"/=",
            tok!"|=", tok!"^^=", tok!"^=",
            tok!"~="))
        {
            auto node = allocate!AssignExpression;
            node.line = current().line;
            node.column = current().column;
            node.ternaryExpression = ternary;
            node.operator = advance().type;
            mixin (nullCheck!`node.assignExpression = parseAssignExpression()`);
            return node;
        }
        return ternary;
    }

    /**
     * Parses an AssocArrayLiteral
     *
     * $(GRAMMAR $(RULEDEF assocArrayLiteral):
     *     $(LITERAL '[') $(RULE keyValuePairs) $(LITERAL ']')
     *     ;)
     */
    AssocArrayLiteral parseAssocArrayLiteral()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        mixin (simpleParse!(AssocArrayLiteral, tok!"[",
            "keyValuePairs|parseKeyValuePairs", tok!"]"));
    }

    /**
     * Parses an AtAttribute
     *
     * $(GRAMMAR $(RULEDEF atAttribute):
     *       $(LITERAL '@') $(LITERAL Identifier)
     *     | $(LITERAL '@') $(LITERAL Identifier) $(LITERAL '$(LPAREN)') $(RULE argumentList)? $(LITERAL '$(RPAREN)')
     *     | $(LITERAL '@') $(LITERAL '$(LPAREN)') $(RULE argumentList) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    AtAttribute parseAtAttribute()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!AtAttribute;
        auto start = expect(tok!"@");
        mixin (nullCheck!`start`);
        if (!moreTokens)
        {
            error(`"(", or identifier expected`);
            return null;
        }
        node.startLocation = start.index;
        switch (current.type)
        {
        case tok!"identifier":
            node.identifier = advance();
            if (currentIs(tok!"("))
            {
                advance(); // (
                mixin (nullCheck!`node.argumentList = parseArgumentList()`);
                expect(tok!")");
            }
            break;
        case tok!"(":
            advance();
            mixin (nullCheck!`node.argumentList = parseArgumentList()`);
            expect(tok!")");
            break;
        default:
            error(`"(", or identifier expected`);
            return null;
        }
        if (moreTokens) node.endLocation = current().index;
        return node;
    }

    /**
     * Parses an Attribute
     *
     * $(GRAMMAR $(RULEDEF attribute):
     *     | $(RULE pragmaExpression)
     *     | $(RULE alignAttribute)
     *     | $(RULE deprecated)
     *     | $(RULE atAttribute)
     *     | $(RULE linkageAttribute)
     *     | $(LITERAL 'export')
     *     | $(LITERAL 'package')
     *     | $(LITERAL 'private')
     *     | $(LITERAL 'protected')
     *     | $(LITERAL 'public')
     *     | $(LITERAL 'static')
     *     | $(LITERAL 'extern')
     *     | $(LITERAL 'abstract')
     *     | $(LITERAL 'final')
     *     | $(LITERAL 'override')
     *     | $(LITERAL 'synchronized')
     *     | $(LITERAL 'auto')
     *     | $(LITERAL 'scope')
     *     | $(LITERAL 'const')
     *     | $(LITERAL 'immutable')
     *     | $(LITERAL 'inout')
     *     | $(LITERAL 'shared')
     *     | $(LITERAL '__gshared')
     *     | $(LITERAL 'nothrow')
     *     | $(LITERAL 'pure')
     *     | $(LITERAL 'ref')
     *     ;)
     */
    Attribute parseAttribute()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Attribute;
        switch (current.type)
        {
        case tok!"pragma":
            mixin (nullCheck!`node.pragmaExpression = parsePragmaExpression()`);
            break;
        case tok!"deprecated":
            mixin (nullCheck!`node.deprecated_ = parseDeprecated()`);
            break;
        case tok!"align":
            mixin (nullCheck!`node.alignAttribute = parseAlignAttribute()`);
            break;
        case tok!"@":
            mixin (nullCheck!`node.atAttribute = parseAtAttribute()`);
            break;
        case tok!"extern":
            if (peekIs(tok!"("))
            {
                mixin (nullCheck!`node.linkageAttribute = parseLinkageAttribute()`);
                break;
            }
            else
                goto case;
        case tok!"package":
            node.attribute = advance();
            if (currentIs(tok!"("))
            {
                expect(tok!"(");
                mixin (nullCheck!`node.identifierChain = parseIdentifierChain()`);
                expect(tok!")");
            }
            break;
        case tok!"private":
        case tok!"protected":
        case tok!"public":
        case tok!"export":
        case tok!"static":
        case tok!"abstract":
        case tok!"final":
        case tok!"override":
        case tok!"synchronized":
        case tok!"auto":
        case tok!"scope":
        case tok!"const":
        case tok!"immutable":
        case tok!"inout":
        case tok!"shared":
        case tok!"__gshared":
        case tok!"nothrow":
        case tok!"pure":
        case tok!"ref":
            node.attribute = advance();
            break;
        default:
            deallocate(node);
            return null;
        }
        return node;
    }

    /**
     * Parses an AttributeDeclaration
     *
     * $(GRAMMAR $(RULEDEF attributeDeclaration):
     *     $(RULE attribute) $(LITERAL ':')
     *     ;)
     */
    AttributeDeclaration parseAttributeDeclaration(Attribute attribute = null)
    {
        auto node = allocate!AttributeDeclaration;
        node.attribute = attribute is null ? parseAttribute() : attribute;
        expect(tok!":");
        return node;
    }

    /**
     * Parses an AutoDeclaration
     *
     * $(GRAMMAR $(RULEDEF autoDeclaration):
     *     $(RULE storageClass) $(LITERAL Identifier) $(LITERAL '=') $(RULE initializer) ($(LITERAL ',') $(LITERAL Identifier) $(LITERAL '=') $(RULE initializer))* $(LITERAL ';')
     *     ;)
     */
    AutoDeclaration parseAutoDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!AutoDeclaration;
        node.comment = comment;
        comment = null;
        if (isStorageClass())
            mixin (nullCheck!`node.storageClass = parseStorageClass()`);
        Token[] identifiers;
        Initializer[] initializers;
        do
        {
            auto ident = expect(tok!"identifier");
            mixin (nullCheck!`ident`);
            identifiers ~= *ident;
            mixin (nullCheck!`expect(tok!"=")`);
            auto init = parseInitializer();
            mixin (nullCheck!`init`);
            initializers ~= init;
            if (currentIs(tok!","))
                advance();
            else
                break;
        } while (moreTokens());
        node.identifiers = ownArray(identifiers);
        node.initializers = ownArray(initializers);
        mixin (nullCheck!`expect(tok!";")`);
        return node;
    }

    /**
     * Parses a BlockStatement
     *
     * $(GRAMMAR $(RULEDEF blockStatement):
     *     $(LITERAL '{') $(RULE declarationsAndStatements)? $(LITERAL '}')
     *     ;)
     */
    BlockStatement parseBlockStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!BlockStatement;
        auto openBrace = expect(tok!"{");
        mixin (nullCheck!`openBrace`);
        node.startLocation = openBrace.index;
        if (!currentIs(tok!"}"))
        {
            mixin (nullCheck!`node.declarationsAndStatements = parseDeclarationsAndStatements()`);
        }
        auto closeBrace = expect(tok!"}");
        if (closeBrace !is null)
            node.endLocation = closeBrace.index;
        else
        {
            trace("Could not find end of block statement.");
            node.endLocation = size_t.max;
        }

        return node;
    }

    /**
     * Parses a BodyStatement
     *
     * $(GRAMMAR $(RULEDEF bodyStatement):
     *     $(LITERAL 'body') $(RULE blockStatement)
     *     ;)
     */
    BodyStatement parseBodyStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        mixin (simpleParse!(BodyStatement, tok!"body",
            "blockStatement|parseBlockStatement"));
    }

    /**
     * Parses a BreakStatement
     *
     * $(GRAMMAR $(RULEDEF breakStatement):
     *     $(LITERAL 'break') $(LITERAL Identifier)? $(LITERAL ';')
     *     ;)
     */
    BreakStatement parseBreakStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        expect(tok!"break");
        auto node = allocate!BreakStatement;
        switch (current.type)
        {
        case tok!"identifier":
            node.label = advance();
            mixin (nullCheck!`expect(tok!";")`);
            break;
        case tok!";":
            advance();
            break;
        default:
            error("Identifier or semicolon expected following \"break\"");
            return null;
        }
        return node;
    }

    /**
     * Parses a BaseClass
     *
     * $(GRAMMAR $(RULEDEF baseClass):
     *     $(RULE type2)
     *     ;)
     */
    BaseClass parseBaseClass()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!BaseClass;
        if (current.type.isProtection())
        {
            warn("Use of base class protection is deprecated.");
            advance();
        }
        if ((node.type2 = parseType2()) is null)
        {
            deallocate(node);
            return null;
        }
        return node;
    }

    /**
     * Parses a BaseClassList
     *
     * $(GRAMMAR $(RULEDEF baseClassList):
     *     $(RULE baseClass) ($(LITERAL ',') $(RULE baseClass))*
     *     ;)
     */
    BaseClassList parseBaseClassList()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseCommaSeparatedRule!(BaseClassList, BaseClass)();
    }

    /**
     * Parses an BuiltinType
     *
     * $(GRAMMAR $(RULEDEF builtinType):
     *      $(LITERAL 'bool')
     *    | $(LITERAL 'byte')
     *    | $(LITERAL 'ubyte')
     *    | $(LITERAL 'short')
     *    | $(LITERAL 'ushort')
     *    | $(LITERAL 'int')
     *    | $(LITERAL 'uint')
     *    | $(LITERAL 'long')
     *    | $(LITERAL 'ulong')
     *    | $(LITERAL 'char')
     *    | $(LITERAL 'wchar')
     *    | $(LITERAL 'dchar')
     *    | $(LITERAL 'float')
     *    | $(LITERAL 'double')
     *    | $(LITERAL 'real')
     *    | $(LITERAL 'ifloat')
     *    | $(LITERAL 'idouble')
     *    | $(LITERAL 'ireal')
     *    | $(LITERAL 'cfloat')
     *    | $(LITERAL 'cdouble')
     *    | $(LITERAL 'creal')
     *    | $(LITERAL 'void')
     *    ;)
     */
    IdType parseBuiltinType()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return advance().type;
    }

    /**
     * Parses a CaseRangeStatement
     *
     * $(GRAMMAR $(RULEDEF caseRangeStatement):
     *     $(LITERAL 'case') $(RULE assignExpression) $(LITERAL ':') $(LITERAL '...') $(LITERAL 'case') $(RULE assignExpression) $(LITERAL ':') $(RULE declarationsAndStatements)
     *     ;)
     */
    CaseRangeStatement parseCaseRangeStatement(ExpressionNode low)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!CaseRangeStatement;
        assert (low !is null);
        node.low = low;
        mixin (nullCheck!`expect(tok!":")`);
        mixin (nullCheck!`expect(tok!"..")`);
        expect(tok!"case");
        mixin (nullCheck!`node.high = parseAssignExpression()`);
        mixin (nullCheck!`expect(tok!":")`);
        mixin (nullCheck!`node.declarationsAndStatements = parseDeclarationsAndStatements()`);
        return node;
    }

    /**
     * Parses an CaseStatement
     *
     * $(GRAMMAR $(RULEDEF caseStatement):
     *     $(LITERAL 'case') $(RULE argumentList) $(LITERAL ':') $(RULE declarationsAndStatements)
     *     ;)
     */
    CaseStatement parseCaseStatement(ArgumentList argumentList = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!CaseStatement;
        node.argumentList = argumentList;
        mixin (nullCheck!`expect(tok!":")`);
        mixin (nullCheck!`node.declarationsAndStatements = parseDeclarationsAndStatements()`);
        return node;
    }

    /**
     * Parses a CastExpression
     *
     * $(GRAMMAR $(RULEDEF castExpression):
     *     $(LITERAL 'cast') $(LITERAL '$(LPAREN)') ($(RULE type) | $(RULE castQualifier))? $(LITERAL '$(RPAREN)') $(RULE unaryExpression)
     *     ;)
     */
    CastExpression parseCastExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!CastExpression;
        expect(tok!"cast");
        mixin (nullCheck!`expect(tok!"(")`);
        if (!currentIs(tok!")"))
        {
            if (isCastQualifier())
                mixin (nullCheck!`node.castQualifier = parseCastQualifier()`);
            else
                mixin (nullCheck!`node.type = parseType()`);
        }
        mixin (nullCheck!`expect(tok!")")`);
        mixin (nullCheck!`node.unaryExpression = parseUnaryExpression()`);
        return node;
    }

    /**
     * Parses a CastQualifier
     *
     * $(GRAMMAR $(RULEDEF castQualifier):
     *      $(LITERAL 'const')
     *    | $(LITERAL 'const') $(LITERAL 'shared')
     *    | $(LITERAL 'immutable')
     *    | $(LITERAL 'inout')
     *    | $(LITERAL 'inout') $(LITERAL 'shared')
     *    | $(LITERAL 'shared')
     *    | $(LITERAL 'shared') $(LITERAL 'const')
     *    | $(LITERAL 'shared') $(LITERAL 'inout')
     *    ;)
     */
    CastQualifier parseCastQualifier()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!CastQualifier;
        switch (current.type)
        {
        case tok!"inout":
        case tok!"const":
            node.first = advance();
            if (currentIs(tok!"shared"))
                node.second = advance();
            break;
        case tok!"shared":
            node.first = advance();
            if (currentIsOneOf(tok!"const", tok!"inout"))
                node.second = advance();
            break;
        case tok!"immutable":
            node.first = advance();
            break;
        default:
            error("const, immutable, inout, or shared expected");
            return null;
        }
        return node;
    }

    /**
     * Parses a Catch
     *
     * $(GRAMMAR $(RULEDEF catch):
     *     $(LITERAL 'catch') $(LITERAL '$(LPAREN)') $(RULE type) $(LITERAL Identifier)? $(LITERAL '$(RPAREN)') $(RULE declarationOrStatement)
     *     ;)
     */
    Catch parseCatch()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Catch;
        expect(tok!"catch");
        mixin (nullCheck!`expect(tok!"(")`);
        mixin (nullCheck!`node.type = parseType()`);
        if (currentIs(tok!"identifier"))
            node.identifier = advance();
        mixin (nullCheck!`expect(tok!")")`);
        mixin (nullCheck!`node.declarationOrStatement = parseDeclarationOrStatement()`);
        return node;
    }

    /**
     * Parses a Catches
     *
     * $(GRAMMAR $(RULEDEF catches):
     *       $(RULE catch)+
     *     | $(RULE catch)* $(RULE lastCatch)
     *     ;)
     */
    Catches parseCatches()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Catches;
        Catch[] catches;
        while (moreTokens())
        {
            if (!currentIs(tok!"catch"))
                break;
            if (peekIs(tok!"("))
                catches ~= parseCatch();
            else
            {
                node.lastCatch  = parseLastCatch();
                break;
            }
        }
        node.catches = ownArray(catches);
        return node;
    }

    /**
     * Parses a ClassDeclaration
     *
     * $(GRAMMAR $(RULEDEF classDeclaration):
     *       $(LITERAL 'class') $(LITERAL Identifier) $(LITERAL ';')
     *     | $(LITERAL 'class') $(LITERAL Identifier) ($(LITERAL ':') $(RULE baseClassList))? $(RULE structBody)
     *     | $(LITERAL 'class') $(LITERAL Identifier) $(RULE templateParameters) $(RULE constraint)? ($(RULE structBody) | $(LITERAL ';'))
     *     | $(LITERAL 'class') $(LITERAL Identifier) $(RULE templateParameters) $(RULE constraint)? ($(LITERAL ':') $(RULE baseClassList))? $(RULE structBody)
     *     | $(LITERAL 'class') $(LITERAL Identifier) $(RULE templateParameters) ($(LITERAL ':') $(RULE baseClassList))? $(RULE constraint)? $(RULE structBody)
     *     ;)
     */
    ClassDeclaration parseClassDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!ClassDeclaration;
        expect(tok!"class");
        mixin (PARSE_INTERFACE_OR_CLASS);
    }

    /**
     * Parses a CmpExpression
     *
     * $(GRAMMAR $(RULEDEF cmpExpression):
     *       $(RULE shiftExpression)
     *     | $(RULE equalExpression)
     *     | $(RULE identityExpression)
     *     | $(RULE relExpression)
     *     | $(RULE inExpression)
     *     ;)
     */
    ExpressionNode parseCmpExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto shift = parseShiftExpression();
        if (shift is null)
            return null;
        if (!moreTokens())
            return shift;
        switch (current.type)
        {
        case tok!"is":
            auto node = allocate!CmpExpression;
            mixin (nullCheck!`node.identityExpression = parseIdentityExpression(shift)`);
            return node;
        case tok!"in":
            auto node = allocate!CmpExpression;
            mixin (nullCheck!`node.inExpression = parseInExpression(shift)`);
            return node;
        case tok!"!":
            auto node = allocate!CmpExpression;
            if (peekIs(tok!"is"))
                mixin (nullCheck!`node.identityExpression = parseIdentityExpression(shift)`);
            else if (peekIs(tok!"in"))
                mixin (nullCheck!`node.inExpression = parseInExpression(shift)`);
            return node;
        case tok!"<":
        case tok!"<=":
        case tok!">":
        case tok!">=":
        case tok!"!<>=":
        case tok!"!<>":
        case tok!"<>":
        case tok!"<>=":
        case tok!"!>":
        case tok!"!>=":
        case tok!"!<":
        case tok!"!<=":
            auto node = allocate!CmpExpression;
            mixin (nullCheck!`node.relExpression = parseRelExpression(shift)`);
            return node;
        case tok!"==":
        case tok!"!=":
            auto node = allocate!CmpExpression;
            mixin (nullCheck!`node.equalExpression = parseEqualExpression(shift)`);
            return node;
        default:
            return shift;
        }
    }

    /**
     * Parses a CompileCondition
     *
     * $(GRAMMAR $(RULEDEF compileCondition):
     *       $(RULE versionCondition)
     *     | $(RULE debugCondition)
     *     | $(RULE staticIfCondition)
     *     ;)
     */
    CompileCondition parseCompileCondition()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!CompileCondition;
        switch (current.type)
        {
        case tok!"version":
            mixin (nullCheck!`node.versionCondition = parseVersionCondition()`);
            break;
        case tok!"debug":
            mixin (nullCheck!`node.debugCondition = parseDebugCondition()`);
            break;
        case tok!"static":
            mixin (nullCheck!`node.staticIfCondition = parseStaticIfCondition()`);
            break;
        default:
            error(`"version", "debug", or "static" expected`);
            return null;
        }
        return node;
    }

    /**
     * Parses a ConditionalDeclaration
     *
     * $(GRAMMAR $(RULEDEF conditionalDeclaration):
     *       $(RULE compileCondition) $(RULE declaration)
     *     | $(RULE compileCondition) $(LITERAL ':') $(RULE declaration)+
     *     | $(RULE compileCondition) $(RULE declaration) $(LITERAL 'else') $(RULE declaration)
     *     ;)
     */
    ConditionalDeclaration parseConditionalDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!ConditionalDeclaration;
        mixin (nullCheck!`node.compileCondition = parseCompileCondition()`);

        Declaration[] trueDeclarations;
        if (currentIs(tok!":"))
        {
            advance();
            while (moreTokens() && !currentIs(tok!"}"))
            {
                auto b = setBookmark();
                auto d = parseDeclaration();
                if (d !is null)
                {
                    abandonBookmark(b);
                    trueDeclarations ~= d;
                }
                else
                {
                    goToBookmark(b);
                    deallocate(node);
                    return null;
                }
            }
            node.trueDeclarations = ownArray(trueDeclarations);
            return node;
        }

        auto dec = parseDeclaration(suppressMessages > 0);
        mixin (nullCheck!`dec`);
        trueDeclarations ~= dec;
        node.trueDeclarations = ownArray(trueDeclarations);

        if (currentIs(tok!"else"))
            advance();
        else
            return node;

        if ((node.falseDeclaration = parseDeclaration(suppressMessages > 0)) is null)
        {
            deallocate(node);
            return null;
        }
        return node;
    }

    /**
     * Parses a ConditionalStatement
     *
     * $(GRAMMAR $(RULEDEF conditionalStatement):
     *     $(RULE compileCondition) $(RULE declarationOrStatement) ($(LITERAL 'else') $(RULE declarationOrStatement))?
     *     ;)
     */
    ConditionalStatement parseConditionalStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!ConditionalStatement;
        mixin (nullCheck!`node.compileCondition = parseCompileCondition()`);
        mixin (nullCheck!`node.trueStatement = parseDeclarationOrStatement()`);
        if (currentIs(tok!"else"))
        {
            advance();
            mixin (nullCheck!`node.falseStatement = parseDeclarationOrStatement()`);
        }
        return node;
    }

    /**
     * Parses a Constraint
     *
     * $(GRAMMAR $(RULEDEF constraint):
     *     $(LITERAL 'if') $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    Constraint parseConstraint()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Constraint;
        mixin (nullCheck!`expect(tok!"if")`);
        mixin (nullCheck!`expect(tok!"(")`);
        mixin (nullCheck!`node.expression = parseExpression()`);
        mixin (nullCheck!`expect(tok!")")`);
        return node;
    }

    /**
     * Parses a Constructor
     *
     * $(GRAMMAR $(RULEDEF constructor):
     *       $(LITERAL 'this') $(RULE templateParameters)? $(RULE parameters) $(RULE memberFunctionAttribute)* $(RULE constraint)? ($(RULE functionBody) | $(LITERAL ';'))
     *     ;)
     */
    Constructor parseConstructor()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        Constructor node = allocate!Constructor;
        node.comment = comment;
        comment = null;
        auto t = expect(tok!"this");
        mixin (nullCheck!`t`);
        node.location = t.index;
        node.line = t.line;
        node.column = t.column;
        auto p = peekPastParens();
        bool isTemplate = false;
        if (p !is null && p.type == tok!"(")
        {
            isTemplate = true;
            mixin (nullCheck!`node.templateParameters = parseTemplateParameters()`);
        }
        mixin (nullCheck!`node.parameters = parseParameters()`);
        mixin (nullCheck!`node.parameters`);

        MemberFunctionAttribute[] memberFunctionAttributes;
        while (moreTokens() && currentIsMemberFunctionAttribute())
            memberFunctionAttributes ~= parseMemberFunctionAttribute();
        node.memberFunctionAttributes = ownArray(memberFunctionAttributes);

        if (isTemplate && currentIs(tok!"if"))
            mixin (nullCheck!`node.constraint = parseConstraint()`);

        if (currentIs(tok!";"))
            advance();
        else
        {
            mixin (nullCheck!`node.functionBody = parseFunctionBody()`);
            mixin (nullCheck!`node.functionBody`);
        }

        return node;
    }

    /**
     * Parses an ContinueStatement
     *
     * $(GRAMMAR $(RULEDEF continueStatement):
     *     $(LITERAL 'continue') $(LITERAL Identifier)? $(LITERAL ';')
     *     ;)
     */
    ContinueStatement parseContinueStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        if (expect(tok!"continue") is null) return null;
        auto node = allocate!ContinueStatement;
        switch (current.type)
        {
        case tok!"identifier":
            node.label = advance();
            mixin (nullCheck!`expect(tok!";")`);
            break;
        case tok!";":
            advance();
            break;
        default:
            error(`Identifier or semicolon expected following "continue"`);
            return null;
        }
        return node;
    }

    /**
     * Parses a DebugCondition
     *
     * $(GRAMMAR $(RULEDEF debugCondition):
     *     $(LITERAL 'debug') ($(LITERAL '$(LPAREN)') ($(LITERAL IntegerLiteral) | $(LITERAL Identifier)) $(LITERAL '$(RPAREN)'))?
     *     ;)
     */
    DebugCondition parseDebugCondition()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!DebugCondition;
        mixin (nullCheck!`expect(tok!"debug")`);
        if (currentIs(tok!"("))
        {
            advance();
            if (currentIsOneOf(tok!"intLiteral", tok!"identifier"))
                node.identifierOrInteger = advance();
            else
            {
                error(`Integer literal or identifier expected`);
                return null;
            }
            mixin (nullCheck!`expect(tok!")")`);
        }
        return node;
    }

    /**
     * Parses a DebugSpecification
     *
     * $(GRAMMAR $(RULEDEF debugSpecification):
     *     $(LITERAL 'debug') $(LITERAL '=') ($(LITERAL Identifier) | $(LITERAL IntegerLiteral)) $(LITERAL ';')
     *     ;)
     */
    DebugSpecification parseDebugSpecification()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!DebugSpecification;
        mixin (nullCheck!`expect(tok!"debug")`);
        mixin (nullCheck!`expect(tok!"=")`);
        if (currentIsOneOf(tok!"identifier", tok!"intLiteral"))
            node.identifierOrInteger = advance();
        else
        {
            error("Integer literal or identifier expected");
            return null;
        }
        mixin (nullCheck!`expect(tok!";")`);
        return node;
    }

    /**
     * Parses a Declaration
     *
     * Params: strict = if true, do not return partial AST nodes on errors.
     *
     * $(GRAMMAR $(RULEDEF declaration):
     *     $(RULE attribute)* $(declaration2)
     *     ;
     * $(RULEDEF declaration2):
     *       $(RULE aliasDeclaration)
     *     | $(RULE aliasThisDeclaration)
     *     | $(RULE anonymousEnumDeclaration)
     *     | $(RULE attributeDeclaration)
     *     | $(RULE classDeclaration)
     *     | $(RULE conditionalDeclaration)
     *     | $(RULE constructor)
     *     | $(RULE debugSpecification)
     *     | $(RULE destructor)
     *     | $(RULE enumDeclaration)
     *     | $(RULE eponymousTemplateDeclaration)
     *     | $(RULE functionDeclaration)
     *     | $(RULE importDeclaration)
     *     | $(RULE interfaceDeclaration)
     *     | $(RULE invariant)
     *     | $(RULE mixinDeclaration)
     *     | $(RULE mixinTemplateDeclaration)
     *     | $(RULE pragmaDeclaration)
     *     | $(RULE sharedStaticConstructor)
     *     | $(RULE sharedStaticDestructor)
     *     | $(RULE staticAssertDeclaration)
     *     | $(RULE staticConstructor)
     *     | $(RULE staticDestructor)
     *     | $(RULE structDeclaration)
     *     | $(RULE templateDeclaration)
     *     | $(RULE unionDeclaration)
     *     | $(RULE unittest)
     *     | $(RULE variableDeclaration)
     *     | $(RULE versionSpecification)
     *     | $(LITERAL '{') $(RULE declaration)+ $(LITERAL '}')
     *     ;)
     */
    Declaration parseDeclaration(bool strict = false)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Declaration;
        if (!moreTokens)
        {
            error("declaration expected instead of EOF");
            return null;
        }
        comment = current.comment;
        Attribute[] attributes;
        do
        {
            if (!isAttribute())
                break;
            auto attr = parseAttribute();
            if (attr is null)
            {
                error("attribute is null");
                break;
            }
            if (currentIs(tok!":"))
            {
                node.attributeDeclaration = parseAttributeDeclaration(attr);
                node.attributes = ownArray(attributes);
                return node;
            }
            else
                attributes ~= attr;
        } while (moreTokens());
        node.attributes = ownArray(attributes);

        if (!moreTokens)
        {
            error("declaration expected instead of EOF");
            deallocate(node);
            return null;
        }

        switch (current.type)
        {
        case tok!"asm":
        case tok!"break":
        case tok!"case":
        case tok!"continue":
        case tok!"default":
        case tok!"do":
        case tok!"for":
        case tok!"foreach":
        case tok!"foreach_reverse":
        case tok!"goto":
        case tok!"if":
        case tok!"return":
        case tok!"switch":
        case tok!"throw":
        case tok!"try":
        case tok!"while":
        case tok!"assert":
            goto default;
        case tok!";":
            // http://d.puremagic.com/issues/show_bug.cgi?id=4559
            warn("Empty declaration");
            advance();
            break;
        case tok!"{":
            advance();
            Declaration[] declarations;
            while (moreTokens() && !currentIs(tok!"}"))
            {
                auto declaration = parseDeclaration();
                if (declaration !is null)
                    declarations ~= declaration;
                else if (strict)
                    return null;
            }
            node.declarations = ownArray(declarations);
            mixin (nullCheck!`expect(tok!"}")`);
            break;
        case tok!"alias":
            if (startsWith(tok!"alias", tok!"identifier", tok!"this"))
                mixin (nullCheck!`node.aliasThisDeclaration = parseAliasThisDeclaration()`);
            else
                mixin (nullCheck!`node.aliasDeclaration = parseAliasDeclaration()`);
            break;
        case tok!"class":
            mixin (nullCheck!`node.classDeclaration = parseClassDeclaration()`);
            break;
        case tok!"this":
            if (startsWith(tok!"this", tok!"(", tok!"this", tok!")"))
            {
                mixin (nullCheck!`node.postblit = parsePostblit()`);
                mixin (nullCheck!`node.postblit`);
            }
            else
            {
                mixin (nullCheck!`node.constructor = parseConstructor()`);
                mixin (nullCheck!`node.constructor`);
            }
            break;
        case tok!"~":
            mixin (nullCheck!`node.destructor = parseDestructor()`);
            mixin (nullCheck!`node.destructor`);
            break;
        case tok!"enum":
            auto b = setBookmark();
            advance(); // enum
            if (currentIsOneOf(tok!":", tok!"{"))
            {
                goToBookmark(b);
                mixin (nullCheck!`node.anonymousEnumDeclaration = parseAnonymousEnumDeclaration()`);
            }
            else if (currentIs(tok!"identifier"))
            {
                advance();
                if (currentIs(tok!"("))
                {
                    skipParens(); // ()
                    if (currentIs(tok!"("))
                        skipParens();
                    if (!currentIs(tok!"="))
                    {
                        goToBookmark(b);
                        node.functionDeclaration = parseFunctionDeclaration(null, true, node.attributes);
                        mixin (nullCheck!`node.functionDeclaration`);
                    }
                    else
                    {
                        goToBookmark(b);
                        mixin (nullCheck!`node.eponymousTemplateDeclaration = parseEponymousTemplateDeclaration()`);
                    }
                }
                else if (currentIsOneOf(tok!":", tok!"{", tok!";"))
                {
            enumDeclaration:
                    goToBookmark(b);
                    mixin (nullCheck!`node.enumDeclaration = parseEnumDeclaration()`);
                }
                else
                {
                    auto eq = currentIs(tok!"=");
                    goToBookmark(b);
                    mixin (nullCheck!`node.variableDeclaration = parseVariableDeclaration(null, eq)`);
                }
            }
            else
            {
                goToBookmark(b);
                mixin (nullCheck!`node.variableDeclaration = parseVariableDeclaration()`);
            }
            break;
        case tok!"import":
            mixin (nullCheck!`node.importDeclaration = parseImportDeclaration()`);
            break;
        case tok!"interface":
            mixin (nullCheck!`node.interfaceDeclaration = parseInterfaceDeclaration()`);
            break;
        case tok!"mixin":
            if (peekIs(tok!"template"))
                mixin (nullCheck!`node.mixinTemplateDeclaration = parseMixinTemplateDeclaration()`);
            else
            {
                auto b = setBookmark();
                advance();
                if (currentIs(tok!"("))
                {
                    auto t = peekPastParens();
                    if (t !is null && t.type == tok!";")
                    {
                        goToBookmark(b);
                        mixin (nullCheck!`node.mixinDeclaration = parseMixinDeclaration()`);
                    }
                    else
                    {
                        goToBookmark(b);
                        error("Declaration expected");
                        deallocate(node);
                        return null;
                    }
                }
                else
                {
                    goToBookmark(b);
                    mixin (nullCheck!`node.mixinDeclaration = parseMixinDeclaration()`);
                }
            }
            break;
        case tok!"pragma":
            mixin (nullCheck!`node.pragmaDeclaration = parsePragmaDeclaration()`);
            break;
        case tok!"shared":
            if (startsWith(tok!"shared", tok!"static", tok!"this"))
                mixin (nullCheck!`node.sharedStaticConstructor = parseSharedStaticConstructor()`);
            else if (startsWith(tok!"shared", tok!"static", tok!"~"))
                mixin (nullCheck!`node.sharedStaticDestructor = parseSharedStaticDestructor()`);
            else
                goto type;
            break;
        case tok!"static":
            if (peekIs(tok!"this"))
                mixin (nullCheck!`node.staticConstructor = parseStaticConstructor()`);
            else if (peekIs(tok!"~"))
                mixin (nullCheck!`node.staticDestructor = parseStaticDestructor()`);
            else if (peekIs(tok!"if"))
                mixin (nullCheck!`node.conditionalDeclaration = parseConditionalDeclaration()`);
            else if (peekIs(tok!"assert"))
                mixin (nullCheck!`node.staticAssertDeclaration = parseStaticAssertDeclaration()`);
            else
                goto type;
            break;
        case tok!"struct":
            mixin (nullCheck!`node.structDeclaration = parseStructDeclaration()`);
            break;
        case tok!"template":
            mixin (nullCheck!`node.templateDeclaration = parseTemplateDeclaration()`);
            break;
        case tok!"union":
            mixin (nullCheck!`node.unionDeclaration = parseUnionDeclaration()`);
            break;
        case tok!"invariant":
            mixin (nullCheck!`node.invariant_ = parseInvariant()`);
            break;
        case tok!"unittest":
            mixin (nullCheck!`node.unittest_ = parseUnittest()`);
            break;
        case tok!"identifier":
            if (node.attributes.length > 0)
            {
                if (peekIs(tok!"="))
                    node.variableDeclaration = parseVariableDeclaration(null, true, node.attributes);
                else if (peekIs(tok!"("))
                {
                    auto b = setBookmark();
                    advance();
                    auto t = peekPastParens();
                    goToBookmark(b);
                    if (t !is null && *t == tok!"=")
                        node.variableDeclaration = parseVariableDeclaration(null, true, node.attributes);
                    else
                        node.functionDeclaration = parseFunctionDeclaration(null, true, node.attributes);
                }
                else
                    goto type;
            }
            else
                goto type;
            break;
        case tok!".":
        case tok!"const":
        case tok!"immutable":
        case tok!"inout":
        case tok!"scope":
        case tok!"typeof":
        case tok!"__vector":
        mixin (BUILTIN_TYPE_CASES);
        type:
            Type type = parseType();
            if (type is null || !currentIs(tok!"identifier"))
            {
                trace("Returning null on %d".format(__LINE__));
                deallocate(node);
                return null;
            }
            if (peekIs(tok!"("))
            {
                auto b = setBookmark();
                advance();
                auto t = peekPastParens();
                goToBookmark(b);
                if (t is null)
                    return null;
                if (*t != tok!"=")
                    node.functionDeclaration = parseFunctionDeclaration(type, false, node.attributes);
                else
                    node.variableDeclaration = parseVariableDeclaration(type, false, node.attributes);
            }
            else
            {
                if ((node.variableDeclaration = parseVariableDeclaration(type)) is null)
                {
                    deallocate(node);
                    return null;
                }
            }

            break;
        case tok!"version":
            if (peekIs(tok!"("))
            {
                if ((node.conditionalDeclaration = parseConditionalDeclaration()) is null)
                {
                    deallocate(node);
                    return null;
                }
            }
            else if (peekIs(tok!"="))
            {
                if ((node.versionSpecification = parseVersionSpecification()) is null)
                {
                    deallocate(node);
                    return null;
                }
            }
            else
            {
                error(`"=" or "(" expected following "version"`);
                deallocate(node);
                return null;
            }
            break;
        case tok!"debug":
            if (peekIs(tok!"="))
            {
                mixin (nullCheck!`node.debugSpecification = parseDebugSpecification()`);
                mixin (nullCheck!`node.debugSpecification`);
            }
            else
            {
                mixin (nullCheck!`node.conditionalDeclaration = parseConditionalDeclaration()`);
                mixin (nullCheck!`node.conditionalDeclaration`);
            }
            break;
        default:
            error("Declaration expected");
            deallocate(node);
            return null;
        }
        return node;
    }

    /**
     * Parses DeclarationsAndStatements
     *
     * $(GRAMMAR $(RULEDEF declarationsAndStatements):
     *     $(RULE declarationOrStatement)+
     *     ;)
     */
    DeclarationsAndStatements parseDeclarationsAndStatements()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!DeclarationsAndStatements;
        DeclarationOrStatement[] declarationsAndStatements;
        while (!currentIsOneOf(tok!"}", tok!"else") && moreTokens() && suppressedErrorCount <= MAX_ERRORS)
        {
            if (currentIs(tok!"while"))
            {
                auto b = setBookmark();
                scope (exit) goToBookmark(b);
                advance();
                if (currentIs(tok!"("))
                {
                    auto p = peekPastParens();
                    if (p !is null && *p == tok!";")
                        break;
                }
            }
            auto dos = parseDeclarationOrStatement();
            if (dos !is null)
                declarationsAndStatements ~= dos;
            else if (suppressMessages > 0)
            {
                deallocate(node);
                return null;
            }
        }
        node.declarationsAndStatements = ownArray(declarationsAndStatements);
        return node;
    }

    /**
     * Parses a DeclarationOrStatement
     *
     * $(GRAMMAR $(RULEDEF declarationOrStatement):
     *       $(RULE declaration)
     *     | $(RULE statement)
     *     ;)
     */
    DeclarationOrStatement parseDeclarationOrStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!DeclarationOrStatement;
        // "Any ambiguities in the grammar between Statements and
        // Declarations are resolved by the declarations taking precedence."
        auto b = setBookmark();
        auto d = parseDeclaration(true);
        if (d !is null)
        {
            abandonBookmark(b);
            node.declaration = d;
        }
        else
        {
            goToBookmark(b);
            mixin (nullCheck!`node.statement = parseStatement()`);
        }
        return node;
    }

    /**
     * Parses a Declarator
     *
     * $(GRAMMAR $(RULEDEF declarator):
     *       $(LITERAL Identifier)
     *     | $(LITERAL Identifier) $(LITERAL '=') $(RULE initializer)
     *     | $(LITERAL Identifier) $(RULE templateParameters) $(LITERAL '=') $(RULE initializer)
     *     ;)
     */
    Declarator parseDeclarator()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Declarator;
        auto id = expect(tok!"identifier");
        mixin (nullCheck!`id`);
        node.name = *id;
        if (currentIs(tok!"[")) // dmd doesn't accept pointer after identifier
        {
            warn("C-style array declaration.");
            TypeSuffix[] typeSuffixes;
            while (moreTokens() && currentIs(tok!"["))
            {
                auto suffix = parseTypeSuffix();
                if (suffix !is null)
                    typeSuffixes ~= suffix;
                else
                {
                    deallocate(node);
                    return null;
                }
            }
            node.cstyle = ownArray(typeSuffixes);
        }
        if (currentIs(tok!"("))
        {
            mixin (nullCheck!`(node.templateParameters = parseTemplateParameters())`);
            mixin (nullCheck!`expect(tok!"=")`);
            mixin (nullCheck!`(node.initializer = parseInitializer())`);
        }
        else if (currentIs(tok!"="))
        {
            advance();
            mixin (nullCheck!`node.initializer = parseInitializer()`);
        }
        return node;
    }

    /**
     * Parses a DefaultStatement
     *
     * $(GRAMMAR $(RULEDEF defaultStatement):
     *     $(LITERAL 'default') $(LITERAL ':') $(RULE declarationsAndStatements)
     *     ;)
     */
    DefaultStatement parseDefaultStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!DefaultStatement;
        mixin (nullCheck!`expect(tok!"default")`);
        mixin (nullCheck!`expect(tok!":")`);
        mixin (nullCheck!`node.declarationsAndStatements = parseDeclarationsAndStatements()`);
        return node;
    }

    /**
     * Parses a DeleteExpression
     *
     * $(GRAMMAR $(RULEDEF deleteExpression):
     *     $(LITERAL 'delete') $(RULE unaryExpression)
     *     ;)
     */
    DeleteExpression parseDeleteExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!DeleteExpression;
        node.line = current.line;
        node.column = current.column;
        mixin (nullCheck!`expect(tok!"delete")`);
        mixin (nullCheck!`node.unaryExpression = parseUnaryExpression()`);
        return node;
    }

    /**
     * Parses a Deprecated attribute
     *
     * $(GRAMMAR $(RULEDEF deprecated):
     *     $(LITERAL 'deprecated') ($(LITERAL '$(LPAREN)') $(LITERAL StringLiteral) $(LITERAL '$(RPAREN)'))?
     *     ;)
     */
    Deprecated parseDeprecated()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Deprecated;
        mixin (nullCheck!`expect(tok!"deprecated")`);
        if (currentIs(tok!"("))
        {
            advance();
            auto s = expect(tok!"stringLiteral");
            if (s is null)
            {
                if (currentIs(tok!")"))
                    advance();
                deallocate(node);
                return null;
            }
            node.stringLiteral = *s;
            mixin (nullCheck!`expect(tok!")")`);
        }
        return node;
    }

    /**
     * Parses a Destructor
     *
     * $(GRAMMAR $(RULEDEF destructor):
     *     $(LITERAL '~') $(LITERAL 'this') $(LITERAL '$(LPAREN)') $(LITERAL '$(RPAREN)') $(RULE memberFunctionAttribute)* ($(RULE functionBody) | $(LITERAL ';'))
     *     ;)
     */
    Destructor parseDestructor()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Destructor;
        node.comment = comment;
        comment = null;
        mixin (nullCheck!`expect(tok!"~")`);
        if (!moreTokens)
        {
            error("'this' expected");
            deallocate(node);
            return null;
        }
        node.index = current.index;
        node.line = current.line;
        node.column = current.column;
        mixin (nullCheck!`expect(tok!"this")`);
        mixin (nullCheck!`expect(tok!"(")`);
        mixin (nullCheck!`expect(tok!")")`);
        if (currentIs(tok!";"))
            advance();
        else
        {
            MemberFunctionAttribute[] memberFunctionAttributes;
            while (moreTokens() && currentIsMemberFunctionAttribute())
                memberFunctionAttributes ~= parseMemberFunctionAttribute();
            node.memberFunctionAttributes = ownArray(memberFunctionAttributes);
            mixin (nullCheck!`node.functionBody = parseFunctionBody()`);
        }
        return node;
    }

    /**
     * Parses a DoStatement
     *
     * $(GRAMMAR $(RULEDEF doStatement):
     *     $(LITERAL 'do') $(RULE statementNoCaseNoDefault) $(LITERAL 'while') $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)') $(LITERAL ';')
     *     ;)
     */
    DoStatement parseDoStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!DoStatement;
        mixin (nullCheck!`expect(tok!"do")`);
        mixin (nullCheck!`node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault()`);
        mixin (nullCheck!`expect(tok!"while")`);
        mixin (nullCheck!`expect(tok!"(")`);
        mixin (nullCheck!`node.expression = parseExpression()`);
        mixin (nullCheck!`expect(tok!")")`);
        mixin (nullCheck!`expect(tok!";")`);
        return node;
    }

    /**
     * Parses an EnumBody
     *
     * $(GRAMMAR $(RULEDEF enumBody):
     *     $(LITERAL '{') $(RULE enumMember) ($(LITERAL ',') $(RULE enumMember)?)* $(LITERAL '}')
     *     ;)
     */
    EnumBody parseEnumBody()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        EnumBody node = allocate!EnumBody;
        auto open = expect(tok!"{");
        mixin (nullCheck!`open`);
        node.startLocation = open.index;
        EnumMember[] enumMembers;
        while (moreTokens())
        {
            if (currentIs(tok!","))
            {
                if (enumMembers.length > 0 && enumMembers[$ - 1].comment is null)
                    enumMembers[$ - 1].comment = current.trailingComment;
                advance();
                continue;
            }
            else if (currentIs(tok!"}"))
            {
                if (enumMembers.length > 0 && enumMembers[$ - 1].comment is null)
                    enumMembers[$ - 1].comment = tokens[index - 1].trailingComment;
                break;
            }
            else
            {
                auto member = parseEnumMember();
                if (member is null)
                    return null;
                enumMembers ~= member;
            }
        }
        node.enumMembers = ownArray(enumMembers);
        auto close = expect (tok!"}");
        if (close !is null)
            node.endLocation = close.index;
        return node;
    }

    /**
     * $(GRAMMAR $(RULEDEF anonymousEnumMember):
     *       $(Rule type) $(LITERAL identifier) $(LITERAL '=') $(RULE assignExpression)
     *     | $(LITERAL identifier) $(LITERAL '=') $(RULE assignExpression)
     *     | $(LITERAL identifier)
     *     ;)
     */
    AnonymousEnumMember parseAnonymousEnumMember(bool typeAllowed)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!AnonymousEnumMember;

        if (currentIs(tok!"identifier") && peekIsOneOf(tok!",", tok!"=", tok!"}"))
        {
            node.comment = current.comment;
            mixin (tokenCheck!(`node.name`, `identifier`));
            if (currentIs(tok!"="))
            {
                advance(); // =
                goto assign;
            }
        }
        else if (typeAllowed)
        {
            node.comment = current.comment;
            mixin (nullCheck!`node.type = parseType()`);
            mixin (tokenCheck!(`node.name`, `identifier`));
            mixin (nullCheck!`expect(tok!"=")`);
    assign:
            mixin (nullCheck!`node.assignExpression = parseAssignExpression()`);
        }
        else
        {
            error("Cannot specify anonymous enum member type if anonymous enum has a base type.");
            deallocate(node);
            return null;
        }
        return node;
    }

    /**
     * $(GRAMMAR $(RULEDEF anonymousEnumDeclaration):
     *     $(LITERAL 'enum') ($(LITERAL ':') $(RULE type))? $(LITERAL '{') $(RULE anonymousEnumMember)+ $(LITERAL '}')
     *     ;)
     */
    AnonymousEnumDeclaration parseAnonymousEnumDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!AnonymousEnumDeclaration;
        mixin (nullCheck!`expect(tok!"enum")`);
        immutable bool hasBaseType = currentIs(tok!":");
        if (hasBaseType)
        {
            advance();
            mixin (nullCheck!`node.baseType = parseType()`);
        }
        mixin (nullCheck!`expect(tok!"{")`);
        AnonymousEnumMember[] members;
        while (moreTokens())
        {
            if (currentIs(tok!","))
            {
                if (members.length > 0 && members[$ - 1].comment is null)
                    members[$ - 1].comment = current.trailingComment;
                advance();
                continue;
            }
            else if (currentIs(tok!"}"))
            {
                if (members.length > 0 && members[$ - 1].comment is null)
                    members[$ - 1].comment = tokens[index - 1].trailingComment;
                break;
            }
            else
            {
                auto member = parseAnonymousEnumMember(!hasBaseType);
                if (member is null)
                    return null;
                members ~= member;
            }
        }
        node.members = ownArray(members);
        mixin (nullCheck!`expect(tok!"}")`);
        return node;
    }

    /**
     * Parses an EnumDeclaration
     *
     * $(GRAMMAR $(RULEDEF enumDeclaration):
     *       $(LITERAL 'enum') $(LITERAL Identifier) ($(LITERAL ':') $(RULE type))? $(LITERAL ';')
     *     | $(LITERAL 'enum') $(LITERAL Identifier) ($(LITERAL ':') $(RULE type))? $(RULE enumBody)
     *     ;)
     */
    EnumDeclaration parseEnumDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!EnumDeclaration;
        mixin (nullCheck!`expect(tok!"enum")`);
        mixin (tokenCheck!(`node.name`, `identifier`));
        node.comment = comment;
        comment = null;
        if (currentIs(tok!";"))
        {
            advance();
            return node;
        }
        if (currentIs(tok!":"))
        {
            advance(); // skip ':'
            mixin (nullCheck!`node.type = parseType()`);
        }
        mixin (nullCheck!`node.enumBody = parseEnumBody()`);
        return node;
    }

    /**
     * Parses an EnumMember
     *
     * $(GRAMMAR $(RULEDEF enumMember):
     *       $(LITERAL Identifier)
     *     | $(LITERAL Identifier) $(LITERAL '=') $(RULE assignExpression)
     *     ;)
     */
    EnumMember parseEnumMember()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!EnumMember;
        node.comment = current.comment;
        mixin (tokenCheck!(`node.name`, `identifier`));
        if (currentIs(tok!"="))
        {
            advance();
            mixin (nullCheck!`node.assignExpression = parseAssignExpression()`);
        }
        return node;
    }

    /**
     * Parses an EponymousTemplateDeclaration
     *
     * $(GRAMMAR $(RULEDEF eponymousTemplateDeclaration):
     *     $(LITERAL 'enum') $(LITERAL Identifier) $(RULE templateParameters) $(LITERAL '=') $(RULE assignExpression) $(LITERAL ';')
     *     ;)
     */
    EponymousTemplateDeclaration parseEponymousTemplateDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!EponymousTemplateDeclaration;
        advance(); // enum
        auto ident = expect(tok!"identifier");
        mixin (nullCheck!`ident`);
        node.name = *ident;
        mixin (nullCheck!`node.templateParameters = parseTemplateParameters()`);
        expect(tok!"=");
        node.assignExpression = parseAssignExpression();
        if (node.assignExpression is null)
            mixin (nullCheck!`node.type = parseType()`);
        expect(tok!";");
        return node;
    }

    /**
     * Parses an EqualExpression
     *
     * $(GRAMMAR $(RULEDEF equalExpression):
     *     $(RULE shiftExpression) ($(LITERAL '==') | $(LITERAL '!=')) $(RULE shiftExpression)
     *     ;)
     */
    EqualExpression parseEqualExpression(ExpressionNode shift = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!EqualExpression;
        mixin (nullCheck!`node.left = shift is null ? parseShiftExpression() : shift`);
        if (currentIsOneOf(tok!"==", tok!"!="))
            node.operator = advance().type;
        mixin (nullCheck!`node.right = parseShiftExpression()`);
        return node;
    }

    /**
     * Parses an Expression
     *
     * $(GRAMMAR $(RULEDEF expression):
     *     $(RULE assignExpression) ($(LITERAL ',') $(RULE assignExpression))*
     *     ;)
     */
    Expression parseExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        if (suppressedErrorCount > MAX_ERRORS) return null;
        return parseCommaSeparatedRule!(Expression, AssignExpression, true)();
    }

    /**
     * Parses an ExpressionStatement
     *
     * $(GRAMMAR $(RULEDEF expressionStatement):
     *     $(RULE expression) $(LITERAL ';')
     *     ;)
     */
    ExpressionStatement parseExpressionStatement(Expression expression = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!ExpressionStatement;
        node.expression = expression is null ? parseExpression() : expression;
        if (node.expression is null || expect(tok!";") is null) { deallocate(node); return null; }
        return node;
    }

    /**
     * Parses a FinalSwitchStatement
     *
     * $(GRAMMAR $(RULEDEF finalSwitchStatement):
     *     $(LITERAL 'final') $(RULE switchStatement)
     *     ;)
     */
    FinalSwitchStatement parseFinalSwitchStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        mixin (simpleParse!(FinalSwitchStatement, tok!"final", "switchStatement|parseSwitchStatement"));
    }

    /**
     * Parses a Finally
     *
     * $(GRAMMAR $(RULEDEF finally):
     *     $(LITERAL 'finally') $(RULE declarationOrStatement)
     *     ;)
     */
    Finally parseFinally()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Finally;
        mixin (nullCheck!`expect(tok!"finally")`);
        mixin (nullCheck!`node.declarationOrStatement = parseDeclarationOrStatement()`);
        return node;
    }

    /**
     * Parses a ForStatement
     *
     * $(GRAMMAR $(RULEDEF forStatement):
     *     $(LITERAL 'for') $(LITERAL '$(LPAREN)') ($(RULE declaration) | $(RULE statementNoCaseNoDefault)) $(RULE expression)? $(LITERAL ';') $(RULE expression)? $(LITERAL '$(RPAREN)') $(RULE declarationOrStatement)
     *     ;)
     */
    ForStatement parseForStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!ForStatement;
        mixin (nullCheck!`expect(tok!"for")`);
        if (!moreTokens) node.startIndex = current().index;
        mixin (nullCheck!`expect(tok!"(")`);

        if (currentIs(tok!";"))
            advance();
        else
            mixin (nullCheck!`node.initialization = parseDeclarationOrStatement()`);

        if (currentIs(tok!";"))
            advance();
        else
        {
            mixin (nullCheck!`node.test = parseExpression()`);
            expect(tok!";");
        }

        if (!currentIs(tok!")"))
             mixin (nullCheck!`node.increment = parseExpression()`);

        mixin (nullCheck!`expect(tok!")")`);
        if (currentIs(tok!"}"))
        {
            error("Statement expected", false);
            return node; // this line makes DCD better
        }
        mixin (nullCheck!`node.declarationOrStatement = parseDeclarationOrStatement()`);
        return node;
    }

    /**
     * Parses a ForeachStatement
     *
     * $(GRAMMAR $(RULEDEF foreachStatement):
     *       ($(LITERAL 'foreach') | $(LITERAL 'foreach_reverse')) $(LITERAL '$(LPAREN)') $(RULE foreachTypeList) $(LITERAL ';') $(RULE expression) $(LITERAL '$(RPAREN)') $(RULE declarationOrStatement)
     *     | ($(LITERAL 'foreach') | $(LITERAL 'foreach_reverse')) $(LITERAL '$(LPAREN)') $(RULE foreachType) $(LITERAL ';') $(RULE expression) $(LITERAL '..') $(RULE expression) $(LITERAL '$(RPAREN)') $(RULE declarationOrStatement)
     *     ;)
     */
    ForeachStatement parseForeachStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        ForeachStatement node = allocate!ForeachStatement;
        if (currentIsOneOf(tok!"foreach", tok!"foreach_reverse"))
            node.type = advance().type;
        else
        {
            error(`"foreach" or "foreach_reverse" expected`);
            deallocate(node);
            return null;
        }
        node.startIndex = current().index;
        mixin (nullCheck!`expect(tok!"(")`);
        ForeachTypeList feType = parseForeachTypeList();
        mixin (nullCheck!`feType`);
        bool canBeRange = feType.items.length == 1;

        mixin (nullCheck!`expect(tok!";")`);
        mixin (nullCheck!`node.low = parseExpression()`);
        mixin (nullCheck!`node.low`);
        if (currentIs(tok!".."))
        {
            if (!canBeRange)
            {
                error(`Cannot have more than one foreach variable for a foreach range statement`);
                return null;
            }
            advance();
            mixin (nullCheck!`node.high = parseExpression()`);
            node.foreachType = feType.items[0];
            mixin (nullCheck!`node.high`);
        }
        else
        {
            node.foreachTypeList = feType;
        }
        mixin (nullCheck!`expect(tok!")")`);
        if (currentIs(tok!"}"))
        {
            error("Statement expected", false);
            return node; // this line makes DCD better
        }
        mixin (nullCheck!`node.declarationOrStatement = parseDeclarationOrStatement()`);
        return node;
    }

    /**
     * Parses a ForeachType
     *
     * $(GRAMMAR $(RULEDEF foreachType):
     *       $(LITERAL 'ref')? $(RULE typeConstructors)? $(RULE type)? $(LITERAL Identifier)
     *     | $(RULE typeConstructors)? $(LITERAL 'ref')? $(RULE type)? $(LITERAL Identifier)
     *     ;)
     */
    ForeachType parseForeachType()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!ForeachType;
        if (currentIs(tok!"ref"))
        {
            node.isRef = true;
            advance();
        }
        if (currentIsOneOf(tok!"const", tok!"immutable",
            tok!"inout", tok!"shared") && !peekIs(tok!"("))
        {
            trace("\033[01;36mType constructor");
            if ((node.typeConstructors = parseTypeConstructors()) is null)
                return null;
        }
        if (currentIs(tok!"ref"))
        {
            node.isRef = true;
            advance();
        }
        if (currentIs(tok!"identifier") && peekIsOneOf(tok!",", tok!";"))
        {
            node.identifier = advance();
            return node;
        }
        if ((node.type = parseType()) is null) { deallocate(node); return null; }
        auto ident = expect(tok!"identifier");
        if (ident is null) { deallocate(node); return null; }
        node.identifier = *ident;
        return node;
    }

    /**
     * Parses a ForeachTypeList
     *
     * $(GRAMMAR $(RULEDEF foreachTypeList):
     *     $(RULE foreachType) ($(LITERAL ',') $(RULE foreachType))*
     *     ;)
     */
    ForeachTypeList parseForeachTypeList()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseCommaSeparatedRule!(ForeachTypeList, ForeachType)();
    }

    /**
     * Parses a FunctionAttribute
     *
     * $(GRAMMAR $(RULEDEF functionAttribute):
     *       $(RULE atAttribute)
     *     | $(LITERAL 'pure')
     *     | $(LITERAL 'nothrow')
     *     ;)
     */
    FunctionAttribute parseFunctionAttribute(bool validate = true)
    {
        auto node = allocate!FunctionAttribute;
        switch (current.type)
        {
        case tok!"@":
            mixin (nullCheck!`node.atAttribute = parseAtAttribute()`);
            break;
        case tok!"pure":
        case tok!"nothrow":
            node.token = advance();
            break;
        default:
            if (validate)
                error(`@attribute, "pure", or "nothrow" expected`);
            deallocate(node);
            return null;
        }
        return node;
    }

    /**
     * Parses a FunctionBody
     *
     * $(GRAMMAR $(RULEDEF functionBody):
     *       $(RULE blockStatement)
     *     | ($(RULE inStatement) | $(RULE outStatement) | $(RULE outStatement) $(RULE inStatement) | $(RULE inStatement) $(RULE outStatement))? $(RULE bodyStatement)?
     *     ;)
     */
    FunctionBody parseFunctionBody()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!FunctionBody;
        if (currentIs(tok!";"))
        {
            advance();
            return node;
        }
        else if (currentIs(tok!"{"))
        {
            if ((node.blockStatement = parseBlockStatement()) is null)
            {
                deallocate(node);
                return null;
            }
        }
        else
        {
            if (currentIs(tok!"in"))
            {
                mixin (nullCheck!`node.inStatement = parseInStatement()`);
                if (currentIs(tok!"out"))
                    mixin (nullCheck!`node.outStatement = parseOutStatement()`);
            }
            else if (currentIs(tok!"out"))
            {
                mixin (nullCheck!`node.outStatement = parseOutStatement()`);
                if (currentIs(tok!"in"))
                    mixin (nullCheck!`node.inStatement = parseInStatement()`);
            }
            // Allow function bodies without body statements because this is
            // valid inside of interfaces.
            if (currentIs(tok!"body"))
                mixin (nullCheck!`node.bodyStatement = parseBodyStatement()`);
        }
        return node;
    }

    /**
     * Parses a FunctionCallExpression
     *
     * $(GRAMMAR $(RULEDEF functionCallExpression):
     *      $(RULE symbol) $(RULE arguments)
     *      $(RULE unaryExpression) $(RULE arguments)
     *     | $(RULE type) $(RULE arguments)
     *     ;)
     */
    FunctionCallExpression parseFunctionCallExpression(UnaryExpression unary = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!FunctionCallExpression;
        switch (current.type)
        {
        case tok!"const":
        case tok!"immutable":
        case tok!"inout":
        case tok!"shared":
        case tok!"scope":
        case tok!"pure":
        case tok!"nothrow":
            mixin (nullCheck!`node.type = parseType()`);
            mixin (nullCheck!`node.arguments = parseArguments()`);
            break;
        default:
            if (unary !is null)
                node.unaryExpression = unary;
            else
                mixin (nullCheck!`node.unaryExpression = parseUnaryExpression()`);
            if (currentIs(tok!"!"))
                mixin (nullCheck!`node.templateArguments = parseTemplateArguments()`);
            if (unary !is null)
                if ((node.arguments = parseArguments()) is null)
                {
                    deallocate(node);
                    return null;
                }
        }
        if (node.arguments is null)
        {
            deallocate(node);
            return null;
        }
        return node;
    }

    /**
     * Parses a FunctionDeclaration
     *
     * $(GRAMMAR $(RULEDEF functionDeclaration):
     *       ($(RULE storageClass)+ | $(RULE _type)) $(LITERAL Identifier) $(RULE parameters) $(RULE memberFunctionAttribute)* ($(RULE functionBody) | $(LITERAL ';'))
     *     | ($(RULE storageClass)+ | $(RULE _type)) $(LITERAL Identifier) $(RULE templateParameters) $(RULE parameters) $(RULE memberFunctionAttribute)* $(RULE constraint)? ($(RULE functionBody) | $(LITERAL ';'))
     *     ;)
     */
    FunctionDeclaration parseFunctionDeclaration(Type type = null, bool isAuto = false,
        Attribute[] attributes = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!FunctionDeclaration;
        node.comment = comment;
        comment = null;
        MemberFunctionAttribute[] memberFunctionAttributes;

        node.attributes = attributes;

        if (isAuto)
        {
            StorageClass[] storageClasses;
            while (isStorageClass())
            {
                auto s = parseStorageClass();
                if (s is null)
                {
                    deallocate(node);
                    return null;
                }
                else
                    storageClasses ~= s;
            }
            node.storageClasses = storageClasses;


            foreach (a; node.attributes)
            {
                if (a.attribute == tok!"auto")
                    node.hasAuto = true;
                else if (a.attribute == tok!"ref")
                    node.hasRef = true;
                else
                    continue;
            }
        }
        else
        {
            while (moreTokens() && currentIsMemberFunctionAttribute())
                memberFunctionAttributes ~= parseMemberFunctionAttribute();

            node.returnType = type is null ? parseType() : type;
        }

        auto ident = expect(tok!"identifier");
        if (ident is null) { deallocate(node); return null; }

        node.name = *ident;

        if (!currentIs(tok!"("))
        {
            error(`"(" expected`);
            return null;
        }

        assert (currentIs(tok!"("));
        auto p = peekPastParens();
        bool isTemplate = p !is null && p.type == tok!"(";

        if (isTemplate)
            mixin (nullCheck!`node.templateParameters = parseTemplateParameters()`);

        mixin (nullCheck!`node.parameters = parseParameters()`);
        if (node.parameters is null) { deallocate(node); return null; }

        while (moreTokens() && currentIsMemberFunctionAttribute())
            memberFunctionAttributes ~= parseMemberFunctionAttribute();

        if (isTemplate && currentIs(tok!"if"))
            mixin (nullCheck!`node.constraint = parseConstraint()`);

        if (currentIs(tok!";"))
            advance();
        else
            mixin (nullCheck!`node.functionBody = parseFunctionBody()`);
        node.memberFunctionAttributes = ownArray(memberFunctionAttributes);
        return node;
    }

    /**
     * Parses a FunctionLiteralExpression
     *
     * $(GRAMMAR $(RULEDEF functionLiteralExpression):
     *     (($(LITERAL 'function') | $(LITERAL 'delegate')) $(RULE type)?)? ($(RULE parameters) $(RULE functionAttribute)*)? $(RULE functionBody)
     *     ;)
     */
    FunctionLiteralExpression parseFunctionLiteralExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!FunctionLiteralExpression;
        if (currentIsOneOf(tok!"function", tok!"delegate"))
        {
            node.functionOrDelegate = advance().type;
            if (!currentIsOneOf(tok!"(", tok!"in", tok!"body",
                tok!"out", tok!"{"))
            {
                mixin (nullCheck!`node.type = parseType()`);
                if (node.type is null) { deallocate(node); return null; }
            }
        }
        if (currentIs(tok!"("))
        {
            mixin (nullCheck!`node.parameters = parseParameters()`);
            if (node.parameters is null) { deallocate(node); return null; }
            FunctionAttribute[] functionAttributes;
            do
            {
                auto attr = parseFunctionAttribute(false);
                if (attr is null)
                    break;
                else
                    functionAttributes ~= attr;
            } while (moreTokens());
            node.functionAttributes = ownArray(functionAttributes);
        }
        if ((node.functionBody = parseFunctionBody()) is null) { deallocate(node); return null; }
        return node;
    }

    /**
     * Parses a GotoStatement
     *
     * $(GRAMMAR $(RULEDEF gotoStatement):
     *     $(LITERAL 'goto') ($(LITERAL Identifier) | $(LITERAL 'default') | $(LITERAL 'case') $(RULE expression)?) $(LITERAL ';')
     *     ;)
     */
    GotoStatement parseGotoStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!GotoStatement;
        if (expect(tok!"goto") is null) { deallocate(node); return null; }
        switch (current.type)
        {
        case tok!"identifier":
        case tok!"default":
            node.label = advance();
            break;
        case tok!"case":
            node.label = advance();
            if (!currentIs(tok!";"))
                mixin (nullCheck!`node.expression = parseExpression()`);
            break;
        default:
            error(`Identifier, "default", or "case" expected`);
            return null;
        }
        if (expect(tok!";") is null) { deallocate(node); return null; }
        return node;
    }

    /**
     * Parses an IdentifierChain
     *
     * $(GRAMMAR $(RULEDEF identifierChain):
     *     $(LITERAL Identifier) ($(LITERAL '.') $(LITERAL Identifier))*
     *     ;)
     */
    IdentifierChain parseIdentifierChain()
    {
        auto node = allocate!IdentifierChain;
        Token[] identifiers;
        while (moreTokens())
        {
            auto ident = expect(tok!"identifier");
            if (ident is null) { deallocate(node); return null; }
            identifiers ~= *ident;
            if (currentIs(tok!"."))
            {
                advance();
                continue;
            }
            else
                break;
        }
        node.identifiers = ownArray(identifiers);
        return node;
    }

    /**
     * Parses an IdentifierList
     *
     * $(GRAMMAR $(RULEDEF identifierList):
     *     $(LITERAL Identifier) ($(LITERAL ',') $(LITERAL Identifier))*
     *     ;)
     */
    IdentifierList parseIdentifierList()
    {
        auto node = allocate!IdentifierList;
        Token[] identifiers;
        while (moreTokens())
        {
            auto ident = expect(tok!"identifier");
            if (ident is null) { deallocate(node); return null; }
            identifiers ~= *ident;
            if (currentIs(tok!","))
            {
                advance();
                continue;
            }
            else
                break;
        }
        node.identifiers = ownArray(identifiers);
        return node;
    }

    /**
     * Parses an IdentifierOrTemplateChain
     *
     * $(GRAMMAR $(RULEDEF identifierOrTemplateChain):
     *     $(RULE identifierOrTemplateInstance) ($(LITERAL '.') $(RULE identifierOrTemplateInstance))*
     *     ;)
     */
    IdentifierOrTemplateChain parseIdentifierOrTemplateChain()
    {
        auto node = allocate!IdentifierOrTemplateChain;
        IdentifierOrTemplateInstance[] identifiersOrTemplateInstances;
        while (moreTokens())
        {
            auto t = parseIdentifierOrTemplateInstance();
            if (t !is null)
                identifiersOrTemplateInstances ~= t;
            else
                break;
            if (!currentIs(tok!"."))
                break;
            else
                advance();
        }
        node.identifiersOrTemplateInstances = ownArray(identifiersOrTemplateInstances);
        return node;
    }

    /**
     * Parses an IdentifierOrTemplateInstance
     *
     * $(GRAMMAR $(RULEDEF identifierOrTemplateInstance):
     *       $(LITERAL Identifier)
     *     | $(RULE templateInstance)
     *     ;)
     */
    IdentifierOrTemplateInstance parseIdentifierOrTemplateInstance()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!IdentifierOrTemplateInstance;
        if (peekIs(tok!"!") && !startsWith(tok!"identifier",
            tok!"!", tok!"is")
            && !startsWith(tok!"identifier", tok!"!", tok!"in"))
        {
            mixin (nullCheck!`node.templateInstance = parseTemplateInstance()`);
        }
        else
        {
            auto ident = expect(tok!"identifier");
            if (ident is null) { deallocate(node); return null; }
            node.identifier = *ident;
        }
        return node;
    }

    /**
     * Parses an IdentityExpression
     *
     * $(GRAMMAR $(RULEDEF identityExpression):
     *     $(RULE shiftExpression) ($(LITERAL 'is') | ($(LITERAL '!') $(LITERAL 'is'))) $(RULE shiftExpression)
     *     ;)
     */
    ExpressionNode parseIdentityExpression(ExpressionNode shift = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!IdentityExpression;
        node.left = shift is null ? parseShiftExpression() : shift;
        if (currentIs(tok!"!"))
        {
            advance();
            node.negated = true;
        }
        if (expect(tok!"is") is null) { deallocate(node); return null; }
        mixin (nullCheck!`node.right = parseShiftExpression()`);
        return node;
    }

    /**
     * Parses an IfStatement
     *
     * $(GRAMMAR $(RULEDEF ifStatement):
     *     $(LITERAL 'if') $(LITERAL '$(LPAREN)') $(RULE ifCondition) $(LITERAL '$(RPAREN)') $(RULE declarationOrStatement) ($(LITERAL 'else') $(RULE declarationOrStatement))?
     * $(RULEDEF ifCondition):
     *       $(LITERAL 'auto') $(LITERAL Identifier) $(LITERAL '=') $(RULE expression)
     *     | $(RULE type) $(LITERAL Identifier) $(LITERAL '=') $(RULE expression)
     *     | $(RULE expression)
     *     ;)
     */
    IfStatement parseIfStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!IfStatement;
        node.line = current().line;
        node.column = current().column;
        if (expect(tok!"if") is null) { deallocate(node); return null; }
        node.startIndex = current().index;
        if (expect(tok!"(") is null) { deallocate(node); return null; }

        if (currentIs(tok!"auto"))
        {
            advance();
            auto i = expect(tok!"identifier");
            if (i !is null)
                node.identifier = *i;
            expect(tok!"=");
            mixin (nullCheck!`node.expression = parseExpression()`);
        }
        else
        {
            auto b = setBookmark();
            auto t = parseType();
            if (t is null || !currentIs(tok!"identifier")
                || !peekIs(tok!"="))
            {
                goToBookmark(b);
                mixin (nullCheck!`node.expression = parseExpression()`);
            }
            else
            {
                goToBookmark(b);
                mixin (nullCheck!`node.type = parseType()`);
                auto i = expect(tok!"identifier");
                if (i !is null)
                    node.identifier = *i;
                expect(tok!"=");
                mixin (nullCheck!`node.expression = parseExpression()`);
            }
        }

        if (expect(tok!")") is null) { deallocate(node); return null; }
        if (currentIs(tok!"}"))
        {
            error("Statement expected", false);
            return node; // this line makes DCD better
        }
        mixin (nullCheck!`node.thenStatement = parseDeclarationOrStatement()`);
        if (currentIs(tok!"else"))
        {
            advance();
            mixin (nullCheck!`node.elseStatement = parseDeclarationOrStatement()`);
        }
        return node;
    }

    /**
     * Parses an ImportBind
     *
     * $(GRAMMAR $(RULEDEF importBind):
     *     $(LITERAL Identifier) ($(LITERAL '=') $(LITERAL Identifier))?
     *     ;)
     */
    ImportBind parseImportBind()
    {
        auto node = allocate!ImportBind;
        auto ident = expect(tok!"identifier");
        if (ident is null) { deallocate(node); return null; }
        node.left = *ident;
        if (currentIs(tok!"="))
        {
            advance();
            auto id = expect(tok!"identifier");
            if (id is null) { deallocate(node); return null; }
            node.right = *id;
        }
        return node;
    }

    /**
     * Parses ImportBindings
     *
     * $(GRAMMAR $(RULEDEF importBindings):
     *     $(RULE singleImport) $(LITERAL ':') $(RULE importBind) ($(LITERAL ',') $(RULE importBind))*
     *     ;)
     */
    ImportBindings parseImportBindings(SingleImport singleImport)
    {
        auto node = allocate!ImportBindings;
        node.singleImport = singleImport is null ? parseSingleImport() : singleImport;
        if (expect(tok!":") is null) { deallocate(node); return null; }
        ImportBind[] importBinds;
        while (moreTokens())
        {
            auto b = parseImportBind();
            if (b !is null)
            {
                importBinds ~= b;
                if (currentIs(tok!","))
                    advance();
                else
                    break;
            }
            else
                break;
        }
        node.importBinds = ownArray(importBinds);
        return node;
    }

    /**
     * Parses an ImportDeclaration
     *
     * $(GRAMMAR $(RULEDEF importDeclaration):
     *       $(LITERAL 'import') $(RULE singleImport) ($(LITERAL ',') $(RULE singleImport))* ($(LITERAL ',') $(RULE importBindings))? $(LITERAL ';')
     *     | $(LITERAL 'import') $(RULE importBindings) $(LITERAL ';')
     *     ;)
     */
    ImportDeclaration parseImportDeclaration()
    {
        auto node = allocate!ImportDeclaration;
        if (expect(tok!"import") is null) { deallocate(node); return null; }
        SingleImport si = parseSingleImport();
        if (currentIs(tok!":"))
            node.importBindings = parseImportBindings(si);
        else
        {
            SingleImport[] singleImports;
            singleImports ~= si;
            if (currentIs(tok!","))
            {
                advance();
                while (moreTokens())
                {
                    auto single = parseSingleImport();
                    if (single is null)
                        return null;
                    if (currentIs(tok!":"))
                    {
                        node.importBindings = parseImportBindings(single);
                        break;
                    }
                    else
                    {
                        singleImports ~= single;
                        if (currentIs(tok!","))
                            advance();
                        else
                            break;
                    }
                }
            }
            node.singleImports = ownArray(singleImports);
        }
        if (expect(tok!";") is null) { deallocate(node); return null; }
        return node;
    }

    /**
     * Parses an ImportExpression
     *
     * $(GRAMMAR $(RULEDEF importExpression):
     *     $(LITERAL 'import') $(LITERAL '$(LPAREN)') $(RULE assignExpression) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    ImportExpression parseImportExpression()
    {
        auto node = allocate!ImportExpression;
        if (expect(tok!"import") is null) { deallocate(node); return null; }
        if (expect(tok!"(") is null) { deallocate(node); return null; }
        mixin (nullCheck!`node.assignExpression = parseAssignExpression()`);
        if (expect(tok!")") is null) { deallocate(node); return null; }
        return node;
    }

    /**
     * Parses an IndexExpression
     *
     * $(GRAMMAR $(RULEDEF indexExpression):
     *     $(RULE unaryExpression) $(LITERAL '[') $(RULE argumentList) $(LITERAL ']')
     *     ;)
     */
    IndexExpression parseIndexExpression(UnaryExpression unaryExpression = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!IndexExpression;
        node.unaryExpression = unaryExpression is null ? parseUnaryExpression() : unaryExpression;
        if (expect(tok!"[") is null) { deallocate(node); return null; }
        mixin (nullCheck!`node.argumentList = parseArgumentList()`);
        if (expect(tok!"]") is null) { deallocate(node); return null; }
        return node;
    }

    /**
     * Parses an InExpression
     *
     * $(GRAMMAR $(RULEDEF inExpression):
     *     $(RULE shiftExpression) ($(LITERAL 'in') | ($(LITERAL '!') $(LITERAL 'in'))) $(RULE shiftExpression)
     *     ;)
     */
    ExpressionNode parseInExpression(ExpressionNode shift = null)
    {
        auto node = allocate!InExpression;
        node.left = shift is null ? parseShiftExpression() : shift;
        if (currentIs(tok!"!"))
        {
            node.negated = true;
            advance();
        }
        if (expect(tok!"in") is null) { deallocate(node); return null; }
        mixin (nullCheck!`node.right = parseShiftExpression()`);
        return node;
    }

    /**
     * Parses an InStatement
     *
     * $(GRAMMAR $(RULEDEF inStatement):
     *     $(LITERAL 'in') $(RULE blockStatement)
     *     ;)
     */
    InStatement parseInStatement()
    {
        auto node = allocate!InStatement;
        if (expect(tok!"in") is null) { deallocate(node); return null; }
        mixin (nullCheck!`node.blockStatement = parseBlockStatement()`);
        if (node.blockStatement is null)
            return null;
        return node;
    }

    /**
     * Parses an Initializer
     *
     * $(GRAMMAR $(RULEDEF initializer):
     *       $(LITERAL 'void')
     *     | $(RULE nonVoidInitializer)
     *     ;)
     */
    Initializer parseInitializer()
    {
        auto node = allocate!Initializer;
        if (currentIs(tok!"void") && peekIsOneOf(tok!",", tok!";"))
            advance();
        else
            mixin (nullCheck!`node.nonVoidInitializer = parseNonVoidInitializer()`);
        return node;
    }

    /**
     * Parses an InterfaceDeclaration
     *
     * $(GRAMMAR $(RULEDEF interfaceDeclaration):
     *       $(LITERAL 'interface') $(LITERAL Identifier) $(LITERAL ';')
     *     | $(LITERAL 'interface') $(LITERAL Identifier) ($(LITERAL ':') $(RULE baseClassList))? $(RULE structBody)
     *     | $(LITERAL 'interface') $(LITERAL Identifier) $(RULE templateParameters) $(RULE constraint)? ($(LITERAL ':') $(RULE baseClassList))? $(RULE structBody)
     *     | $(LITERAL 'interface') $(LITERAL Identifier) $(RULE templateParameters) ($(LITERAL ':') $(RULE baseClassList))? $(RULE constraint)? $(RULE structBody)
     *     ;)
     */
    InterfaceDeclaration parseInterfaceDeclaration()
    {
        auto node = allocate!InterfaceDeclaration;
        expect(tok!"interface");
        mixin (PARSE_INTERFACE_OR_CLASS);
    }

    /**
     * Parses an Invariant
     *
     * $(GRAMMAR $(RULEDEF invariant):
     *     $(LITERAL 'invariant') ($(LITERAL '$(LPAREN)') $(LITERAL '$(RPAREN)'))? $(RULE blockStatement)
     *     ;)
     */
    Invariant parseInvariant()
    {
        auto node = allocate!Invariant;
        node.index = current.index;
        node.line = current.line;
        if (expect(tok!"invariant") is null) { deallocate(node); return null; }
        if (currentIs(tok!"("))
        {
            advance();
            if (expect(tok!")") is null) { deallocate(node); return null; }
        }
        if ((node.blockStatement = parseBlockStatement()) is null) { deallocate(node); return null; }
        return node;
    }

    /**
     * Parses an IsExpression
     *
     * $(GRAMMAR $(RULEDEF isExpression):
     *     $(LITERAL'is') $(LITERAL '$(LPAREN)') $(RULE type) $(LITERAL identifier)? $(LITERAL '$(RPAREN)')
     *     $(LITERAL'is') $(LITERAL '$(LPAREN)') $(RULE type) $(LITERAL identifier)? $(LITERAL ':') $(RULE typeSpecialization) $(LITERAL '$(RPAREN)')
     *     $(LITERAL'is') $(LITERAL '$(LPAREN)') $(RULE type) $(LITERAL identifier)? $(LITERAL '=') $(RULE typeSpecialization) $(LITERAL '$(RPAREN)')
     *     $(LITERAL'is') $(LITERAL '$(LPAREN)') $(RULE type) $(LITERAL identifier)? $(LITERAL ':') $(RULE typeSpecialization) $(LITERAL ',') $(RULE templateParameterList) $(LITERAL '$(RPAREN)')
     *     $(LITERAL'is') $(LITERAL '$(LPAREN)') $(RULE type) $(LITERAL identifier)? $(LITERAL '=') $(RULE typeSpecialization) $(LITERAL ',') $(RULE templateParameterList) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    IsExpression parseIsExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!IsExpression;
        if (expect(tok!"is") is null) { deallocate(node); return null; }
        if (expect(tok!"(") is null) { deallocate(node); return null; }
        mixin (nullCheck!`node.type = parseType()`);
        if (node.type is null) { deallocate(node); return null; }
        if (currentIs(tok!"identifier"))
            node.identifier = advance();
        if (currentIsOneOf(tok!"==", tok!":"))
        {
            node.equalsOrColon = advance().type;
            mixin (nullCheck!`node.typeSpecialization = parseTypeSpecialization()`);
            if (currentIs(tok!","))
            {
                advance();
                mixin (nullCheck!`node.templateParameterList = parseTemplateParameterList()`);
            }
        }
        if (expect(tok!")") is null) { deallocate(node); return null; }
        return node;
    }

    /**
     * Parses a KeyValuePair
     *
     * $(GRAMMAR $(RULEDEF keyValuePair):
     *     $(RULE assignExpression) $(LITERAL ':') $(RULE assignExpression)
     *     ;)
     */
    KeyValuePair parseKeyValuePair()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!KeyValuePair;
        mixin (nullCheck!`node.key = parseAssignExpression()`);
        if (expect(tok!":") is null) { deallocate(node); return null; }
        mixin (nullCheck!`node.value = parseAssignExpression()`);
        return node;
    }

    /**
     * Parses KeyValuePairs
     *
     * $(GRAMMAR $(RULEDEF keyValuePairs):
     *     $(RULE keyValuePair) ($(LITERAL ',') $(RULE keyValuePair))* $(LITERAL ',')?
     *     ;)
     */
    KeyValuePairs parseKeyValuePairs()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!KeyValuePairs;
        KeyValuePair[] keyValuePairs;
        while (moreTokens())
        {
            auto kvPair = parseKeyValuePair();
            if (kvPair !is null)
                keyValuePairs ~= kvPair;
            if (currentIs(tok!","))
            {
                advance();
                if (currentIs(tok!"]"))
                    break;
            }
            else
                break;
        }
        node.keyValuePairs = ownArray(keyValuePairs);
        return node;
    }

    /**
     * Parses a LabeledStatement
     *
     * $(GRAMMAR $(RULEDEF labeledStatement):
     *     $(LITERAL Identifier) $(LITERAL ':') $(RULE declarationOrStatement)
     *     ;)
     */
    LabeledStatement parseLabeledStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!LabeledStatement;
        auto ident = expect(tok!"identifier");
        if (ident is null) { deallocate(node); return null; }
        node.identifier = *ident;
        expect(tok!":");
        mixin (nullCheck!`node.declarationOrStatement = parseDeclarationOrStatement()`);
        return node;
    }

    /**
     * Parses a LambdaExpression
     *
     * $(GRAMMAR $(RULEDEF lambdaExpression):
     *       $(LITERAL Identifier) $(LITERAL '=>') $(RULE assignExpression)
     *     | $(LITERAL 'function') $(RULE type)? $(RULE parameters) $(RULE functionAttribute)* $(LITERAL '=>') $(RULE assignExpression)
     *     | $(LITERAL 'delegate') $(RULE type)? $(RULE parameters) $(RULE functionAttribute)* $(LITERAL '=>') $(RULE assignExpression)
     *     | $(RULE parameters) $(RULE functionAttribute)* $(LITERAL '=>') $(RULE assignExpression)
     *     ;)
     */
    LambdaExpression parseLambdaExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!LambdaExpression;
        if (currentIsOneOf(tok!"function", tok!"delegate"))
        {
            node.functionType = advance().type;
            if (!currentIs(tok!"("))
                if ((node.returnType = parseType()) is null) { deallocate(node); return null; }
            goto lParen;
        }
        else if (startsWith(tok!"identifier", tok!"=>"))
        {
            node.identifier = advance();
            goto lambda;
        }

        if (currentIs(tok!"("))
        {
        lParen:
            mixin (nullCheck!`node.parameters = parseParameters()`);
            FunctionAttribute[] functionAttributes;
            while (moreTokens())
            {
                auto attribute = parseFunctionAttribute(false);
                if (attribute is null)
                    break;
                functionAttributes ~= attribute;
            }
            node.functionAttributes = ownArray(functionAttributes);
        }
        else
        {
            error(`Identifier, type, or argument list expected`);
            return null;
        }

    lambda:
        if (expect(tok!"=>") is null) { deallocate(node); return null; }
        if ((node.assignExpression = parseAssignExpression()) is null) { deallocate(node); return null; }
        return node;
    }

    /**
     * Parses a LastCatch
     *
     * $(GRAMMAR $(RULEDEF lastCatch):
     *     $(LITERAL 'catch') $(RULE statementNoCaseNoDefault)
     *     ;)
     */
    LastCatch parseLastCatch()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!LastCatch;
        auto t = expect(tok!"catch");
        if (t is null) { deallocate(node); return null; }
        node.line = t.line;
        node.column = t.column;
        if ((node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault()) is null)
            return null;
        return node;
    }

    /**
     * Parses a LinkageAttribute
     *
     * $(GRAMMAR $(RULEDEF linkageAttribute):
     *     $(LITERAL 'extern') $(LITERAL '$(LPAREN)') $(LITERAL Identifier) ($(LITERAL '++') ($(LITERAL ',') $(RULE identifierChain))?)? $(LITERAL '$(RPAREN)')
     *     ;)
     */
    LinkageAttribute parseLinkageAttribute()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!LinkageAttribute;
        expect(tok!"extern");
        expect(tok!"(");
        auto ident = expect(tok!"identifier");
        if (ident is null) { deallocate(node); return null; }
        node.identifier = *ident;
        if (currentIs(tok!"++"))
        {
            advance();
            node.hasPlusPlus = true;
            if (currentIs(tok!","))
            {
                advance();
                mixin (nullCheck!`node.identifierChain = parseIdentifierChain()`);
            }
        }
        expect(tok!")");
        return node;
    }

    /**
     * Parses a MemberFunctionAttribute
     *
     * $(GRAMMAR $(RULEDEF memberFunctionAttribute):
     *       $(RULE functionAttribute)
     *     | $(LITERAL 'immutable')
     *     | $(LITERAL 'inout')
     *     | $(LITERAL 'shared')
     *     | $(LITERAL 'const')
     *     | $(LITERAL 'return')
     *     ;)
     */
    MemberFunctionAttribute parseMemberFunctionAttribute()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!MemberFunctionAttribute;
        switch (current.type)
        {
        case tok!"@":
            mixin (nullCheck!`node.atAttribute = parseAtAttribute()`);
            break;
        case tok!"immutable":
        case tok!"inout":
        case tok!"shared":
        case tok!"const":
        case tok!"pure":
        case tok!"nothrow":
        case tok!"return":
            node.tokenType = advance().type;
            break;
        default:
            error(`Member funtion attribute expected`);
        }
        return node;
    }

    /**
     * Parses a MixinDeclaration
     *
     * $(GRAMMAR $(RULEDEF mixinDeclaration):
     *       $(RULE mixinExpression) $(LITERAL ';')
     *     | $(RULE templateMixinExpression) $(LITERAL ';')
     *     ;)
     */
    MixinDeclaration parseMixinDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!MixinDeclaration;
        if (peekIsOneOf(tok!"identifier", tok!"typeof", tok!"."))
            mixin (nullCheck!`node.templateMixinExpression = parseTemplateMixinExpression()`);
        else if (peekIs(tok!"("))
            mixin (nullCheck!`node.mixinExpression = parseMixinExpression()`);
        else
        {
            error(`"(" or identifier expected`);
            return null;
        }
        expect(tok!";");
        return node;
    }

    /**
     * Parses a MixinExpression
     *
     * $(GRAMMAR $(RULEDEF mixinExpression):
     *     $(LITERAL 'mixin') $(LITERAL '$(LPAREN)') $(RULE assignExpression) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    MixinExpression parseMixinExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!MixinExpression;
        expect(tok!"mixin");
        expect(tok!"(");
        mixin (nullCheck!`node.assignExpression = parseAssignExpression()`);
        expect(tok!")");
        return node;
    }

    /**
     * Parses a MixinTemplateDeclaration
     *
     * $(GRAMMAR $(RULEDEF mixinTemplateDeclaration):
     *     $(LITERAL 'mixin') $(RULE templateDeclaration)
     *     ;)
     */
    MixinTemplateDeclaration parseMixinTemplateDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!MixinTemplateDeclaration;
        if (expect(tok!"mixin") is null) { deallocate(node); return null; }
        mixin (nullCheck!`node.templateDeclaration = parseTemplateDeclaration()`);
        if (node.templateDeclaration is null) { deallocate(node); return null; }
        return node;
    }

    /**
     * Parses a MixinTemplateName
     *
     * $(GRAMMAR $(RULEDEF mixinTemplateName):
     *       $(RULE symbol)
     *     | $(RULE typeofExpression) $(LITERAL '.') $(RULE identifierOrTemplateChain)
     *     ;)
     */
    MixinTemplateName parseMixinTemplateName()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!MixinTemplateName;
        if (currentIs(tok!"typeof"))
        {
            mixin (nullCheck!`node.typeofExpression = parseTypeofExpression()`);
            expect(tok!".");
            mixin (nullCheck!`node.identifierOrTemplateChain = parseIdentifierOrTemplateChain()`);
        }
        else
            mixin (nullCheck!`node.symbol = parseSymbol()`);
        return node;
    }

    /**
     * Parses a Module
     *
     * $(GRAMMAR $(RULEDEF module):
     *     $(RULE moduleDeclaration)? $(RULE declaration)*
     *     ;)
     */
    Module parseModule()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        Module m = allocate!Module;
        if (currentIs(tok!"scriptLine"))
            m.scriptLine = advance();
        bool isDeprecatedModule;
        if (currentIs(tok!"deprecated"))
        {
            auto b = setBookmark();
            advance();
            if (currentIs(tok!"("))
                skipParens();
            isDeprecatedModule = currentIs(tok!"module");
            goToBookmark(b);
        }
        if (currentIs(tok!"module") || isDeprecatedModule)
            m.moduleDeclaration = parseModuleDeclaration();
        Declaration[] declarations;
        while (moreTokens())
        {
            auto declaration = parseDeclaration();
            if (declaration !is null)
                declarations ~= declaration;
        }
        m.declarations = ownArray(declarations);
        return m;
    }

    /**
     * Parses a ModuleDeclaration
     *
     * $(GRAMMAR $(RULEDEF moduleDeclaration):
     *     $(RULE deprecated)? $(LITERAL 'module') $(RULE identifierChain) $(LITERAL ';')
     *     ;)
     */
    ModuleDeclaration parseModuleDeclaration()
    {
        auto node = allocate!ModuleDeclaration;
        if (currentIs(tok!"deprecated"))
            mixin (nullCheck!`node.deprecated_ = parseDeprecated()`);
        auto start = expect(tok!"module");
        if (start is null) { deallocate(node); return null; }
        mixin (nullCheck!`node.moduleName = parseIdentifierChain()`);
        node.comment = start.comment;
        if (node.comment is null)
            node.comment = start.trailingComment;
        comment = null;
        auto end = expect(tok!";");
        node.startLocation = start.index;
        if (end !is null)
            node.endLocation = end.index;
        return node;
    }

    /**
     * Parses a MulExpression.
     *
     * $(GRAMMAR $(RULEDEF mulExpression):
     *       $(RULE powExpression)
     *     | $(RULE mulExpression) ($(LITERAL '*') | $(LITERAL '/') | $(LITERAL '%')) $(RULE powExpression)
     *     ;)
     */
    ExpressionNode parseMulExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(MulExpression, PowExpression,
            tok!"*", tok!"/", tok!"%")();
    }

    /**
     * Parses a NewAnonClassExpression
     *
     * $(GRAMMAR $(RULEDEF newAnonClassExpression):
     *     $(LITERAL 'new') $(RULE arguments)? $(LITERAL 'class') $(RULE arguments)? $(RULE baseClassList)? $(RULE structBody)
     *     ;)
     */
    NewAnonClassExpression parseNewAnonClassExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!NewAnonClassExpression;
        expect(tok!"new");
        if (currentIs(tok!"("))
            mixin (nullCheck!`node.allocatorArguments = parseArguments()`);
        expect(tok!"class");
        if (currentIs(tok!"("))
            mixin (nullCheck!`node.constructorArguments = parseArguments()`);
        if (!currentIs(tok!"{"))
            mixin (nullCheck!`node.baseClassList = parseBaseClassList()`);
        mixin (nullCheck!`node.structBody = parseStructBody()`);
        return node;
    }

    /**
     * Parses a NewExpression
     *
     * $(GRAMMAR $(RULEDEF newExpression):
     *       $(LITERAL 'new') $(RULE type) (($(LITERAL '[') $(RULE assignExpression) $(LITERAL ']')) | $(RULE arguments))?
     *     | $(RULE newAnonClassExpression)
     *     ;)
     */
    NewExpression parseNewExpression()
    {
        // Parse ambiguity.
        // auto a = new int[10];
        //              ^^^^^^^
        // auto a = new int[10];
        //              ^^^****
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!NewExpression;
        if (peekIsOneOf(tok!"class", tok!"("))
        {
            mixin (nullCheck!`node.newAnonClassExpression = parseNewAnonClassExpression()`);
            if (node.newAnonClassExpression is null)
            {
                deallocate(node);
                return null;
            }
        }
        else
        {
            expect(tok!"new");
            if (!moreTokens()) goto fail;
            if ((node.type = parseType()) is null) goto fail;
            if (currentIs(tok!"["))
            {
                advance();
                mixin (nullCheck!`node.assignExpression = parseAssignExpression()`);
                expect(tok!"]");
            }
            else if (currentIs(tok!"("))
                mixin (nullCheck!`node.arguments = parseArguments()`);
        }
        return node;
    fail:
        deallocate(node);
        return null;
    }

    /**
     * Parses a NonVoidInitializer
     *
     * $(GRAMMAR $(RULEDEF nonVoidInitializer):
     *       $(RULE assignExpression)
     *     | $(RULE arrayInitializer)
     *     | $(RULE structInitializer)
     *     ;)
     */
    NonVoidInitializer parseNonVoidInitializer()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!NonVoidInitializer;
        if (currentIs(tok!"{"))
        {
            auto b = peekPastBraces();
            if (b !is null && (b.type == tok!"("))
                mixin (nullCheck!`node.assignExpression = parseAssignExpression()`);
            else
            {
                assert (currentIs(tok!"{"));
                auto m = setBookmark();
                auto initializer = parseStructInitializer();
                if (initializer !is null)
                {
                    node.structInitializer = initializer;
                    abandonBookmark(m);
                }
                else
                {
                    goToBookmark(m);
                    mixin (nullCheck!`node.assignExpression = parseAssignExpression()`);
                }
            }
        }
        else if (currentIs(tok!"["))
        {
            auto b = peekPastBrackets();
            if (b !is null && (b.type == tok!","
                || b.type == tok!")"
                || b.type == tok!"]"
                || b.type == tok!"}"
                || b.type == tok!";"))
            {
                mixin (nullCheck!`node.arrayInitializer = parseArrayInitializer()`);
            }
            else
                mixin (nullCheck!`node.assignExpression = parseAssignExpression()`);
        }
        else
            mixin (nullCheck!`node.assignExpression = parseAssignExpression()`);
        if (node.assignExpression is null && node.arrayInitializer is null
        && node.structInitializer is null)
        {
            deallocate(node);
            return null;
        }
        return node;
    }

    /**
     * Parses Operands
     *
     * $(GRAMMAR $(RULEDEF operands):
     *       $(RULE asmExp)
     *     | $(RULE asmExp) $(LITERAL ',') $(RULE operands)
     *     ;)
     */
    Operands parseOperands()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        Operands node = allocate!Operands;
        ExpressionNode[] expressions;
        while (true)
        {
            ExpressionNode exp = parseAsmExp();
            if (exp is null)
                return null;
            expressions ~= exp;
            if (currentIs(tok!","))
                advance();
            else
                break;
        }
        node.operands = ownArray(expressions);
        return node;
    }

    /**
     * Parses an OrExpression
     *
     * $(GRAMMAR $(RULEDEF orExpression):
     *       $(RULE xorExpression)
     *     | $(RULE orExpression) $(LITERAL '|') $(RULE xorExpression)
     *     ;)
     */
    ExpressionNode parseOrExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(OrExpression, XorExpression,
            tok!"|")();
    }

    /**
     * Parses an OrOrExpression
     *
     * $(GRAMMAR $(RULEDEF orOrExpression):
     *       $(RULE andAndExpression)
     *     | $(RULE orOrExpression) $(LITERAL '||') $(RULE andAndExpression)
     *     ;)
     */
    ExpressionNode parseOrOrExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(OrOrExpression, AndAndExpression,
            tok!"||")();
    }

    /**
     * Parses an OutStatement
     *
     * $(GRAMMAR $(RULEDEF outStatement):
     *     $(LITERAL 'out') ($(LITERAL '$(LPAREN)') $(LITERAL Identifier) $(LITERAL '$(RPAREN)'))? $(RULE blockStatement)
     *     ;)
     */
    OutStatement parseOutStatement()
    {
        auto node = allocate!OutStatement;
        expect(tok!"out");
        if (currentIs(tok!"("))
        {
            advance();
            auto ident = expect(tok!"identifier");
            if (ident is null) { deallocate(node); return null; }
            node.parameter = *ident;
            expect(tok!")");
        }
        mixin (nullCheck!`node.blockStatement = parseBlockStatement()`);
        if (node.blockStatement is null)
            return null;
        return node;
    }

    /**
     * Parses a Parameter
     *
     * $(GRAMMAR $(RULEDEF parameter):
     *     $(RULE parameterAttribute)* $(RULE type)
     *     $(RULE parameterAttribute)* $(RULE type) $(LITERAL Identifier)? $(LITERAL '...')
     *     $(RULE parameterAttribute)* $(RULE type) $(LITERAL Identifier)? ($(LITERAL '=') $(RULE assignExpression))?
     *     ;)
     */
    Parameter parseParameter()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Parameter;
        IdType[] parameterAttributes;
        while (moreTokens())
        {
            IdType type = parseParameterAttribute(false);
            if (type == tok!"")
                break;
            else
                parameterAttributes ~= type;
        }
        node.parameterAttributes = ownArray(parameterAttributes);
        mixin (nullCheck!`node.type = parseType()`);
        if (node.type is null) { deallocate(node); return null; }
        if (currentIs(tok!"identifier"))
        {
            node.name = advance();
            if (currentIs(tok!"..."))
            {
                advance();
                node.vararg = true;
            }
            else if (currentIs(tok!"="))
            {
                advance();
                mixin (nullCheck!`node.default_ = parseAssignExpression()`);
            }
            else if (currentIs(tok!"["))
            {
                TypeSuffix[] typeSuffixes;
                while(moreTokens() && currentIs(tok!"["))
                {
                    auto suffix = parseTypeSuffix();
                    if (suffix !is null)
                        typeSuffixes ~= suffix;
                    else
                        return null;
                }
                node.cstyle = ownArray(typeSuffixes);
            }
        }
        else if (currentIs(tok!"..."))
        {
            node.vararg = true;
            advance();
        }
        else if (currentIs(tok!"="))
        {
            advance();
            mixin (nullCheck!`node.default_ = parseAssignExpression()`);
        }
        return node;
    }

    /**
     * Parses a ParameterAttribute
     *
     * $(GRAMMAR $(RULEDEF parameterAttribute):
     *       $(RULE typeConstructor)
     *     | $(LITERAL 'final')
     *     | $(LITERAL 'in')
     *     | $(LITERAL 'lazy')
     *     | $(LITERAL 'out')
     *     | $(LITERAL 'ref')
     *     | $(LITERAL 'scope')
     *     | $(LITERAL 'auto')
     *     | $(LITERAL 'return')
     *     ;)
     */
    IdType parseParameterAttribute(bool validate = false)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        switch (current.type)
        {
        case tok!"immutable":
        case tok!"shared":
        case tok!"const":
        case tok!"inout":
            if (peekIs(tok!"("))
                return tok!"";
            else
                goto case;
        case tok!"final":
        case tok!"in":
        case tok!"lazy":
        case tok!"out":
        case tok!"ref":
        case tok!"scope":
        case tok!"auto":
        case tok!"return":
            return advance().type;
        default:
            if (validate)
                error("Parameter attribute expected");
            return tok!"";
        }
    }

    /**
     * Parses Parameters
     *
     * $(GRAMMAR $(RULEDEF parameters):
     *       $(LITERAL '$(LPAREN)') $(RULE parameter) ($(LITERAL ',') $(RULE parameter))* ($(LITERAL ',') $(LITERAL '...'))? $(LITERAL '$(RPAREN)')
     *     | $(LITERAL '$(LPAREN)') $(LITERAL '...') $(LITERAL '$(RPAREN)')
     *     | $(LITERAL '$(LPAREN)') $(LITERAL '$(RPAREN)')
     *     ;)
     */
    Parameters parseParameters()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Parameters;
        if (expect(tok!"(") is null) { deallocate(node); return null; }
        Parameter[] parameters;
        if (currentIs(tok!")"))
            goto end;
        if (currentIs(tok!"..."))
        {
            advance();
            node.hasVarargs = true;
            goto end;
        }
        while (moreTokens())
        {
            if (currentIs(tok!"..."))
            {
                advance();
                node.hasVarargs = true;
                break;
            }
            if (currentIs(tok!")"))
                break;
            auto param = parseParameter();
            if (param is null)
                return null;
            parameters ~= param;
            if (currentIs(tok!","))
                advance();
            else
                break;
        }
        node.parameters = ownArray(parameters);
    end:
        if (expect(tok!")") is null)
            return null;
        return node;
    }

    /**
     * Parses a Postblit
     *
     * $(GRAMMAR $(RULEDEF postblit):
     *     $(LITERAL 'this') $(LITERAL '$(LPAREN)') $(LITERAL 'this') $(LITERAL '$(RPAREN)') $(RULE memberFunctionAttribute)* ($(RULE functionBody) | $(LITERAL ';'))
     *     ;)
     */
    Postblit parsePostblit()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Postblit;
        expect(tok!"this");
        expect(tok!"(");
        expect(tok!"this");
        expect(tok!")");
        MemberFunctionAttribute[] memberFunctionAttributes;
        while (currentIsMemberFunctionAttribute())
            memberFunctionAttributes ~= parseMemberFunctionAttribute();
        node.memberFunctionAttributes = ownArray(memberFunctionAttributes);
        if (currentIs(tok!";"))
            advance();
        else
            mixin (nullCheck!`node.functionBody = parseFunctionBody()`);
        return node;
    }

    /**
     * Parses a PowExpression
     *
     * $(GRAMMAR $(RULEDEF powExpression):
     *       $(RULE unaryExpression)
     *     | $(RULE powExpression) $(LITERAL '^^') $(RULE unaryExpression)
     *     ;)
     */
    ExpressionNode parsePowExpression()
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(PowExpression, UnaryExpression,
            tok!"^^")();
    }

    /**
     * Parses a PragmaDeclaration
     *
     * $(GRAMMAR $(RULEDEF pragmaDeclaration):
     *     $(RULE pragmaExpression) $(LITERAL ';')
     *     ;)
     */
    PragmaDeclaration parsePragmaDeclaration()
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!PragmaDeclaration;
        mixin (nullCheck!`node.pragmaExpression = parsePragmaExpression()`);
        expect(tok!";");
        return node;
    }

    /**
     * Parses a PragmaExpression
     *
     * $(GRAMMAR $(RULEDEF pragmaExpression):
     *     $(RULE 'pragma') $(LITERAL '$(LPAREN)') $(LITERAL Identifier) ($(LITERAL ',') $(RULE argumentList))? $(LITERAL '$(RPAREN)')
     *     ;)
     */
    PragmaExpression parsePragmaExpression()
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!PragmaExpression;
        expect(tok!"pragma");
        expect(tok!"(");
        auto ident = expect(tok!"identifier");
        if (ident is null) { deallocate(node); return null; }
        node.identifier = *ident;
        if (currentIs(tok!","))
        {
            advance();
            mixin (nullCheck!`node.argumentList = parseArgumentList()`);
        }
        expect(tok!")");
        return node;
    }

    /**
     * Parses a PrimaryExpression
     *
     * $(GRAMMAR $(RULEDEF primaryExpression):
     *       $(RULE identifierOrTemplateInstance)
     *     | $(LITERAL '.') $(RULE identifierOrTemplateInstance)
     *     | $(RULE typeConstructor) $(LITERAL '$(LPAREN)') $(RULE basicType) $(LITERAL '$(RPAREN)') $(LITERAL '.') $(LITERAL Identifier)
     *     | $(RULE basicType) $(LITERAL '.') $(LITERAL Identifier)
     *     | $(RULE basicType) $(RULE arguments)
     *     | $(RULE typeofExpression)
     *     | $(RULE typeidExpression)
     *     | $(RULE vector)
     *     | $(RULE arrayLiteral)
     *     | $(RULE assocArrayLiteral)
     *     | $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)')
     *     | $(RULE isExpression)
     *     | $(RULE lambdaExpression)
     *     | $(RULE functionLiteralExpression)
     *     | $(RULE traitsExpression)
     *     | $(RULE mixinExpression)
     *     | $(RULE importExpression)
     *     | $(LITERAL '$')
     *     | $(LITERAL 'this')
     *     | $(LITERAL 'super')
     *     | $(LITERAL '_null')
     *     | $(LITERAL '_true')
     *     | $(LITERAL '_false')
     *     | $(LITERAL '___DATE__')
     *     | $(LITERAL '___TIME__')
     *     | $(LITERAL '___TIMESTAMP__')
     *     | $(LITERAL '___VENDOR__')
     *     | $(LITERAL '___VERSION__')
     *     | $(LITERAL '___FILE__')
     *     | $(LITERAL '___LINE__')
     *     | $(LITERAL '___MODULE__')
     *     | $(LITERAL '___FUNCTION__')
     *     | $(LITERAL '___PRETTY_FUNCTION__')
     *     | $(LITERAL IntegerLiteral)
     *     | $(LITERAL FloatLiteral)
     *     | $(LITERAL StringLiteral)+
     *     | $(LITERAL CharacterLiteral)
     *     ;)
     */
    PrimaryExpression parsePrimaryExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!PrimaryExpression;
        if (!moreTokens())
        {
            error("Expected primary statement instead of EOF");
            return null;
        }
        switch (current.type)
        {
        case tok!".":
            node.dot = advance();
            goto case;
        case tok!"identifier":
            if (peekIs(tok!"=>"))
                mixin (nullCheck!`node.lambdaExpression = parseLambdaExpression()`);
            else
                mixin (nullCheck!`node.identifierOrTemplateInstance = parseIdentifierOrTemplateInstance()`);
            break;
        case tok!"immutable":
        case tok!"const":
        case tok!"inout":
        case tok!"shared":
            advance();
            expect(tok!"(");
            mixin (nullCheck!`node.type = parseType()`);
            expect(tok!")");
            expect(tok!".");
            auto ident = expect(tok!"identifier");
            if (ident !is null)
                node.primary = *ident;
            break;
        mixin (BUILTIN_TYPE_CASES);
            node.basicType = advance();
            if (currentIs(tok!"."))
            {
                advance();
                auto t = expect(tok!"identifier");
                if (t !is null)
                    node.primary = *t;
            }
            else if (currentIs(tok!"("))
                if ((node.arguments = parseArguments()) is null)
                {
                    deallocate(node);
                    return null;
                }
            break;
        case tok!"function":
        case tok!"delegate":
            if (peekIs(tok!"("))
            {
                auto b = setBookmark();
                advance(); // function | delegate
                skipParens();
                while (isAttribute())
                    parseAttribute();
                if (currentIs(tok!"=>"))
                {
                    goToBookmark(b);
                    mixin (nullCheck!`node.lambdaExpression = parseLambdaExpression()`);
                    break;
                }
                else
                {
                    goToBookmark(b);
                    mixin (nullCheck!`node.functionLiteralExpression = parseFunctionLiteralExpression()`);
                    break;
                }
            }
            else if (peekIs(tok!"{"))
            {
                mixin (nullCheck!`node.functionLiteralExpression = parseFunctionLiteralExpression()`);
                break;
            }
            else
            {
                auto b = setBookmark();
                advance(); // function or delegate
                if (parseType() is null)
                {
                    goToBookmark(b);
                    goto case;
                }
                if (!currentIs(tok!"("))
                {
                    goToBookmark(b);
                    goto case;
                }
                skipParens();
                while (currentIsMemberFunctionAttribute())
                    parseMemberFunctionAttribute();
                if (!currentIs(tok!"=>"))
                {
                    goToBookmark(b);
                    goto case;
                }
                goToBookmark(b);
                if ((node.lambdaExpression = parseLambdaExpression()) is null) { deallocate(node); return null; }
                return node;
            }
        case tok!"{":
        case tok!"in":
        case tok!"out":
        case tok!"body":
            if ((node.functionLiteralExpression = parseFunctionLiteralExpression()) is null)
            {
                deallocate(node);
                return null;
            }
            break;
        case tok!"typeof":
            mixin (nullCheck!`node.typeofExpression = parseTypeofExpression()`);
            break;
        case tok!"typeid":
            mixin (nullCheck!`node.typeidExpression = parseTypeidExpression()`);
            break;
        case tok!"__vector":
            mixin (nullCheck!`node.vector = parseVector()`);
            break;
        case tok!"[":
            if (isAssociativeArrayLiteral())
                mixin (nullCheck!`node.assocArrayLiteral = parseAssocArrayLiteral()`);
            else
                mixin (nullCheck!`node.arrayLiteral = parseArrayLiteral()`);
            break;
        case tok!"(":
            auto b = setBookmark();
            skipParens();
            while (isAttribute())
                parseAttribute();
            if (currentIs(tok!"=>"))
            {
                goToBookmark(b);
                mixin (nullCheck!`node.lambdaExpression = parseLambdaExpression()`);
            }
            else if (currentIs(tok!"{"))
            {
                goToBookmark(b);
                mixin (nullCheck!`node.functionLiteralExpression = parseFunctionLiteralExpression()`);
            }
            else
            {
                goToBookmark(b);
                advance();
                mixin (nullCheck!`node.expression = parseExpression()`);
                expect(tok!")");
            }
            break;
        case tok!"is":
            mixin (nullCheck!`node.isExpression = parseIsExpression()`);
            break;
        case tok!"__traits":
            mixin (nullCheck!`node.traitsExpression = parseTraitsExpression()`);
            break;
        case tok!"mixin":
            mixin (nullCheck!`node.mixinExpression = parseMixinExpression()`);
            break;
        case tok!"import":
            mixin (nullCheck!`node.importExpression = parseImportExpression()`);
            break;
        case tok!"$":
        case tok!"this":
        case tok!"super":
        case tok!"null":
        case tok!"true":
        case tok!"false":
        mixin (SPECIAL_CASES);
        mixin (LITERAL_CASES);
            if (currentIsOneOf(tok!"stringLiteral", tok!"wstringLiteral", tok!"dstringLiteral"))
            {
                node.primary = advance();
                bool alreadyWarned = false;
                while (currentIsOneOf(tok!"stringLiteral", tok!"wstringLiteral",
                    tok!"dstringLiteral"))
                {
                    if (!alreadyWarned)
                    {
                        warn("Implicit concatenation of string literals");
                        alreadyWarned = true;
                    }
                    node.primary.text~= advance().text;
                }
            }
            else
                node.primary = advance();
            break;
        default:
            deallocate(node);
            error(`Primary expression expected`);
            return null;
        }
        return node;
    }

    /**
     * Parses a Register
     *
     * $(GRAMMAR $(RULEDEF register):
     *       $(LITERAL Identifier)
     *     | $(LITERAL Identifier) $(LITERAL '$(LPAREN)') $(LITERAL IntegerLiteral) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    Register parseRegister()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Register;
        auto ident = expect(tok!"identifier");
        if (ident is null) { deallocate(node); return null; }
        node.identifier = *ident;
        if (currentIs(tok!"("))
        {
            advance();
            auto intLit = expect(tok!"intLiteral");
            if (intLit is null) { deallocate(node); return null; }
            node.intLiteral = *intLit;
            expect(tok!")");
        }
        return node;
    }

    /**
     * Parses a RelExpression
     *
     * $(GRAMMAR $(RULEDEF relExpression):
     *       $(RULE shiftExpression)
     *     | $(RULE relExpression) $(RULE relOperator) $(RULE shiftExpression)
     *     ;
     *$(RULEDEF relOperator):
     *       $(LITERAL '<')
     *     | $(LITERAL '<=')
     *     | $(LITERAL '>')
     *     | $(LITERAL '>=')
     *     | $(LITERAL '!<>=')
     *     | $(LITERAL '!<>')
     *     | $(LITERAL '<>')
     *     | $(LITERAL '<>=')
     *     | $(LITERAL '!>')
     *     | $(LITERAL '!>=')
     *     | $(LITERAL '!<')
     *     | $(LITERAL '!<=')
     *     ;)
     */
    ExpressionNode parseRelExpression(ExpressionNode shift)
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(RelExpression, ShiftExpression,
            tok!"<", tok!"<=", tok!">", tok!">=", tok!"!<>=", tok!"!<>",
            tok!"<>", tok!"<>=", tok!"!>", tok!"!>=", tok!"!>=", tok!"!<",
            tok!"!<=")(shift);
    }

    /**
     * Parses a ReturnStatement
     *
     * $(GRAMMAR $(RULEDEF returnStatement):
     *     $(LITERAL 'return') $(RULE expression)? $(LITERAL ';')
     *     ;)
     */
    ReturnStatement parseReturnStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!ReturnStatement;
        auto start = expect(tok!"return");
        if (start is null) { deallocate(node); return null; }
        node.startLocation = start.index;
        if (!currentIs(tok!";"))
        {
            if ((node.expression = parseExpression()) is null)
            {
                deallocate(node);
                return null;
            }
        }
        auto semicolon = expect(tok!";");
        if (semicolon is null) { deallocate(node); return null; }
        node.endLocation = semicolon.index;
        return node;
    }

    /**
     * Parses a ScopeGuardStatement
     *
     * $(GRAMMAR $(RULEDEF scopeGuardStatement):
     *     $(LITERAL 'scope') $(LITERAL '$(LPAREN)') $(LITERAL Identifier) $(LITERAL '$(RPAREN)') $(RULE statementNoCaseNoDefault)
     *     ;)
     */
    ScopeGuardStatement parseScopeGuardStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!ScopeGuardStatement;
        expect(tok!"scope");
        expect(tok!"(");
        auto ident = expect(tok!"identifier");
        if (ident is null) { deallocate(node); return null; }
        node.identifier = *ident;
        expect(tok!")");
        mixin (nullCheck!`node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault()`);
        return node;
    }

    /**
     * Parses a SharedStaticConstructor
     *
     * $(GRAMMAR $(RULEDEF sharedStaticConstructor):
     *     $(LITERAL 'shared') $(LITERAL 'static') $(LITERAL 'this') $(LITERAL '$(LPAREN)') $(LITERAL '$(RPAREN)') $(RULE functionBody)
     *     ;)
     */
    SharedStaticConstructor parseSharedStaticConstructor()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        mixin (simpleParse!(SharedStaticConstructor, tok!"shared", tok!"static",
            tok!"this", tok!"(", tok!")", "functionBody|parseFunctionBody"));
    }

    /**
     * Parses a SharedStaticDestructor
     *
     * $(GRAMMAR $(RULEDEF sharedStaticDestructor):
     *     $(LITERAL 'shared') $(LITERAL 'static') $(LITERAL '~') $(LITERAL 'this') $(LITERAL '$(LPAREN)') $(LITERAL '$(RPAREN)') $(RULE functionBody)
     *     ;)
     */
    SharedStaticDestructor parseSharedStaticDestructor()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        mixin (simpleParse!(SharedStaticDestructor, tok!"shared", tok!"static",
            tok!"~", tok!"this", tok!"(", tok!")",
            "functionBody|parseFunctionBody"));
    }

    /**
     * Parses a ShiftExpression
     *
     * $(GRAMMAR $(RULEDEF shiftExpression):
     *       $(RULE addExpression)
     *     | $(RULE shiftExpression) ($(LITERAL '<<') | $(LITERAL '>>') | $(LITERAL '>>>')) $(RULE addExpression)
     *     ;)
     */
    ExpressionNode parseShiftExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(ShiftExpression, AddExpression,
            tok!"<<", tok!">>", tok!">>>")();
    }

    /**
     * Parses a SingleImport
     *
     * $(GRAMMAR $(RULEDEF singleImport):
     *     ($(LITERAL Identifier) $(LITERAL '='))? $(RULE identifierChain)
     *     ;)
     */
    SingleImport parseSingleImport()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!SingleImport;
        if (startsWith(tok!"identifier", tok!"="))
        {
            node.rename = advance();
            advance(); // =
        }
        mixin (nullCheck!`node.identifierChain = parseIdentifierChain()`);
        if (node.identifierChain is null)
            return null;
        return node;
    }

    /**
     * Parses a SliceExpression
     *
     * $(GRAMMAR $(RULEDEF sliceExpression):
     *       $(RULE unaryExpression) $(LITERAL '[') $(RULE assignExpression) $(LITERAL '..') $(RULE assignExpression) $(LITERAL ']')
     *     | $(RULE unaryExpression) $(LITERAL '[') $(LITERAL ']')
     *     ;)
     */
    SliceExpression parseSliceExpression(UnaryExpression unary = null)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!SliceExpression;
        node.unaryExpression = unary is null ? parseUnaryExpression() : unary;
        if (expect(tok!"[") is null) { deallocate(node); return null; }
        if (!currentIs(tok!"]"))
        {
            mixin (nullCheck!`node.lower = parseAssignExpression()`);
            if (node.lower is null)
            {
                error("assignExpression expected");
                return null;
            }
            expect(tok!"..");
            mixin (nullCheck!`node.upper = parseAssignExpression()`);
            if (node.upper is null)
            {
                error("assignExpression expected");
                return null;
            }
        }
        if (expect(tok!"]") is null) { deallocate(node); return null; }
        return node;
    }

    /**
     * Parses a Statement
     *
     * $(GRAMMAR $(RULEDEF statement):
     *       $(RULE statementNoCaseNoDefault)
     *     | $(RULE caseStatement)
     *     | $(RULE caseRangeStatement)
     *     | $(RULE defaultStatement)
     *     ;)
     */
    Statement parseStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Statement;
        if (!moreTokens())
        {
            error("Expected statement instead of EOF");
            return null;
        }
        switch (current.type)
        {
        case tok!"case":
            advance();
            auto argumentList = parseArgumentList();
            if (argumentList is null)
            {
                deallocate(node);
                return null;
            }
            if (argumentList.items.length == 1 && startsWith(tok!":", tok!".."))
                node.caseRangeStatement = parseCaseRangeStatement(argumentList.items[0]);
            else
                node.caseStatement = parseCaseStatement(argumentList);
            break;
        case tok!"default":
            mixin (nullCheck!`node.defaultStatement = parseDefaultStatement()`);
            break;
        default:
            if ((node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault()) is null)
            {
                deallocate(node);
                return null;
            }
            break;
        }
        return node;
    }

    /**
     * Parses a StatementNoCaseNoDefault
     *
     * $(GRAMMAR $(RULEDEF statementNoCaseNoDefault):
     *       $(RULE labeledStatement)
     *     | $(RULE blockStatement)
     *     | $(RULE ifStatement)
     *     | $(RULE whileStatement)
     *     | $(RULE doStatement)
     *     | $(RULE forStatement)
     *     | $(RULE foreachStatement)
     *     | $(RULE switchStatement)
     *     | $(RULE finalSwitchStatement)
     *     | $(RULE continueStatement)
     *     | $(RULE breakStatement)
     *     | $(RULE returnStatement)
     *     | $(RULE gotoStatement)
     *     | $(RULE withStatement)
     *     | $(RULE synchronizedStatement)
     *     | $(RULE tryStatement)
     *     | $(RULE throwStatement)
     *     | $(RULE scopeGuardStatement)
     *     | $(RULE asmStatement)
     *     | $(RULE conditionalStatement)
     *     | $(RULE staticAssertStatement)
     *     | $(RULE versionSpecification)
     *     | $(RULE debugSpecification)
     *     | $(RULE expressionStatement)
     *     ;)
     */
    StatementNoCaseNoDefault parseStatementNoCaseNoDefault()
    {
    mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!StatementNoCaseNoDefault;
        node.startLocation = current().index;
        switch (current.type)
        {
        case tok!"{":
            mixin (nullCheck!`node.blockStatement = parseBlockStatement()`);
            break;
        case tok!"if":
            mixin (nullCheck!`node.ifStatement = parseIfStatement()`);
            break;
        case tok!"while":
            mixin (nullCheck!`node.whileStatement = parseWhileStatement()`);
            break;
        case tok!"do":
            mixin (nullCheck!`node.doStatement = parseDoStatement()`);
            break;
        case tok!"for":
            mixin (nullCheck!`node.forStatement = parseForStatement()`);
            break;
        case tok!"foreach":
        case tok!"foreach_reverse":
            mixin (nullCheck!`node.foreachStatement = parseForeachStatement()`);
            break;
        case tok!"switch":
            mixin (nullCheck!`node.switchStatement = parseSwitchStatement()`);
            break;
        case tok!"continue":
            mixin (nullCheck!`node.continueStatement = parseContinueStatement()`);
            break;
        case tok!"break":
            mixin (nullCheck!`node.breakStatement = parseBreakStatement()`);
            break;
        case tok!"return":
            mixin (nullCheck!`node.returnStatement = parseReturnStatement()`);
            break;
        case tok!"goto":
            mixin (nullCheck!`node.gotoStatement = parseGotoStatement()`);
            break;
        case tok!"with":
            mixin (nullCheck!`node.withStatement = parseWithStatement()`);
            break;
        case tok!"synchronized":
            mixin (nullCheck!`node.synchronizedStatement = parseSynchronizedStatement()`);
            break;
        case tok!"try":
            mixin (nullCheck!`node.tryStatement = parseTryStatement()`);
            break;
        case tok!"throw":
            mixin (nullCheck!`node.throwStatement = parseThrowStatement()`);
            break;
        case tok!"scope":
            mixin (nullCheck!`node.scopeGuardStatement = parseScopeGuardStatement()`);
            break;
        case tok!"asm":
            mixin (nullCheck!`node.asmStatement = parseAsmStatement()`);
            break;
        case tok!"final":
            if (peekIs(tok!"switch"))
            {
                mixin (nullCheck!`node.finalSwitchStatement = parseFinalSwitchStatement()`);
                break;
            }
            else
            {
                error(`"switch" expected`);
                return null;
            }
        case tok!"debug":
            if (peekIs(tok!"="))
                mixin (nullCheck!`node.debugSpecification = parseDebugSpecification()`);
            else
                mixin (nullCheck!`node.conditionalStatement = parseConditionalStatement()`);
            break;
        case tok!"version":
            if (peekIs(tok!"="))
                mixin (nullCheck!`node.versionSpecification = parseVersionSpecification()`);
            else
                mixin (nullCheck!`node.conditionalStatement = parseConditionalStatement()`);
            break;
        case tok!"static":
            if (peekIs(tok!"if"))
                mixin (nullCheck!`node.conditionalStatement = parseConditionalStatement()`);
            else if (peekIs(tok!"assert"))
                mixin (nullCheck!`node.staticAssertStatement = parseStaticAssertStatement()`);
            else
            {
                error("'if' or 'assert' expected.");
                return null;
            }
            break;
        case tok!"identifier":
            if (peekIs(tok!":"))
            {
                mixin (nullCheck!`node.labeledStatement = parseLabeledStatement()`);
                break;
            }
            goto default;
        case tok!"delete":
        case tok!"assert":
        default:
            if ((node.expressionStatement = parseExpressionStatement()) is null)
            {
                deallocate(node);
                return null;
            }
            break;
        }
        node.endLocation = tokens[index - 1].index;
        return node;
    }

    /**
     * Parses a StaticAssertDeclaration
     *
     * $(GRAMMAR $(RULEDEF staticAssertDeclaration):
     *     $(RULE staticAssertStatement)
     *     ;)
     */
    StaticAssertDeclaration parseStaticAssertDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        mixin (simpleParse!(StaticAssertDeclaration,
            "staticAssertStatement|parseStaticAssertStatement"));
    }


    /**
     * Parses a StaticAssertStatement
     *
     * $(GRAMMAR $(RULEDEF staticAssertStatement):
     *     $(LITERAL 'static') $(RULE assertExpression) $(LITERAL ';')
     *     ;)
     */
    StaticAssertStatement parseStaticAssertStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        mixin (simpleParse!(StaticAssertStatement,
            tok!"static", "assertExpression|parseAssertExpression", tok!";"));
    }

    /**
     * Parses a StaticConstructor
     *
     * $(GRAMMAR $(RULEDEF staticConstructor):
     *     $(LITERAL 'static') $(LITERAL 'this') $(LITERAL '$(LPAREN)') $(LITERAL '$(RPAREN)') $(RULE functionBody)
     *     ;)
     */
    StaticConstructor parseStaticConstructor()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        mixin (simpleParse!(StaticConstructor, tok!"static", tok!"this",
            tok!"(", tok!")", "functionBody|parseFunctionBody"));
    }

    /**
     * Parses a StaticDestructor
     *
     * $(GRAMMAR $(RULEDEF staticDestructor):
     *     $(LITERAL 'static') $(LITERAL '~') $(LITERAL 'this') $(LITERAL '$(LPAREN)') $(LITERAL '$(RPAREN)') $(RULE functionBody)
     *     ;)
     */
    StaticDestructor parseStaticDestructor()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        mixin (simpleParse!(StaticDestructor, tok!"static", tok!"~", tok!"this",
            tok!"(", tok!")", "functionBody|parseFunctionBody"));
    }

    /**
     * Parses an StaticIfCondition
     *
     * $(GRAMMAR $(RULEDEF staticIfCondition):
     *     $(LITERAL 'static') $(LITERAL 'if') $(LITERAL '$(LPAREN)') $(RULE assignExpression) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    StaticIfCondition parseStaticIfCondition()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        mixin (simpleParse!(StaticIfCondition, tok!"static", tok!"if", tok!"(",
            "assignExpression|parseAssignExpression", tok!")"));
    }

    /**
     * Parses a StorageClass
     *
     * $(GRAMMAR $(RULEDEF storageClass):
     *       $(RULE alignAttribute)
     *     | $(RULE linkageAttribute)
     *     | $(RULE atAttribute)
     *     | $(RULE typeConstructor)
     *     | $(RULE deprecated)
     *     | $(LITERAL 'abstract')
     *     | $(LITERAL 'auto')
     *     | $(LITERAL 'enum')
     *     | $(LITERAL 'extern')
     *     | $(LITERAL 'final')
     *     | $(LITERAL 'nothrow')
     *     | $(LITERAL 'override')
     *     | $(LITERAL 'pure')
     *     | $(LITERAL 'ref')
     *     | $(LITERAL '___gshared')
     *     | $(LITERAL 'scope')
     *     | $(LITERAL 'static')
     *     | $(LITERAL 'synchronized')
     *     ;)
     */
    StorageClass parseStorageClass()
    {
        auto node = allocate!StorageClass;
        switch (current.type)
        {
        case tok!"@":
            mixin (nullCheck!`node.atAttribute = parseAtAttribute()`);
            if (node.atAttribute is null) { deallocate(node); return null; }
            break;
        case tok!"deprecated":
            mixin (nullCheck!`node.deprecated_ = parseDeprecated()`);
            break;
        case tok!"align":
            mixin (nullCheck!`node.alignAttribute = parseAlignAttribute()`);
            break;
        case tok!"extern":
            if (peekIs(tok!"("))
            {
                mixin (nullCheck!`node.linkageAttribute = parseLinkageAttribute()`);
                break;
            }
            else goto case;
        case tok!"const":
        case tok!"immutable":
        case tok!"inout":
        case tok!"shared":
        case tok!"abstract":
        case tok!"auto":
        case tok!"enum":
        case tok!"final":
        case tok!"nothrow":
        case tok!"override":
        case tok!"pure":
        case tok!"ref":
        case tok!"__gshared":
        case tok!"scope":
        case tok!"static":
        case tok!"synchronized":
            node.token = advance();
            break;
        default:
            error(`Storage class expected`);
            return null;
        }
        return node;
    }

    /**
     * Parses a StructBody
     *
     * $(GRAMMAR $(RULEDEF structBody):
     *     $(LITERAL '{') $(RULE declaration)* $(LITERAL '}')
     *     ;)
     */
    StructBody parseStructBody()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!StructBody;
        auto start = expect(tok!"{");
        if (start !is null) node.startLocation = start.index;
        Declaration[] declarations;
        while (!currentIs(tok!"}") && moreTokens())
        {
            auto dec = parseDeclaration();
            if (dec !is null)
                declarations ~= dec;
        }
        node.declarations = ownArray(declarations);
        auto end = expect(tok!"}");
        if (end !is null) node.endLocation = end.index;
        return node;
    }

    /**
     * Parses a StructDeclaration
     *
     * $(GRAMMAR $(RULEDEF structDeclaration):
     *     $(LITERAL 'struct') $(LITERAL Identifier)? ($(RULE templateParameters) $(RULE constraint)? $(RULE structBody) | ($(RULE structBody) | $(LITERAL ';')))
     *     ;)
     */
    StructDeclaration parseStructDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!StructDeclaration;
        auto t = expect(tok!"struct");
        if (currentIs(tok!"identifier"))
            node.name = advance();
        else
        {
            node.name.line = t.line;
            node.name.column = t.column;
        }
        node.comment = comment;
        comment = null;

        if (currentIs(tok!"("))
        {
            mixin (nullCheck!`node.templateParameters = parseTemplateParameters()`);
            if (currentIs(tok!"if"))
                mixin (nullCheck!`node.constraint = parseConstraint()`);
            mixin (nullCheck!`node.structBody = parseStructBody()`);
        }
        else if (currentIs(tok!"{"))
        {
            mixin (nullCheck!`node.structBody = parseStructBody()`);
        }
        else if (currentIs(tok!";"))
            advance();
        else
        {
            error("Template Parameters, Struct Body, or Semicolon expected");
            return null;
        }
        return node;
    }

    /**
     * Parses an StructInitializer
     *
     * $(GRAMMAR $(RULEDEF structInitializer):
     *     $(LITERAL '{') $(RULE structMemberInitializers)? $(LITERAL '}')
     *     ;)
     */
    StructInitializer parseStructInitializer()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!StructInitializer;
        expect(tok!"{");
        if (currentIs(tok!"}"))
            advance();
        else
        {
            mixin (nullCheck!`node.structMemberInitializers = parseStructMemberInitializers()`);
            mixin (nullCheck!`expect(tok!"}")`);
        }
        return node;
    }

    /**
     * Parses a StructMemberInitializer
     *
     * $(GRAMMAR $(RULEDEF structMemberInitializer):
     *     ($(LITERAL Identifier) $(LITERAL ':'))? $(RULE nonVoidInitializer)
     *     ;)
     */
    StructMemberInitializer parseStructMemberInitializer()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!StructMemberInitializer;
        if (startsWith(tok!"identifier", tok!":"))
        {
            node.identifier = tokens[index++];
            index++;
        }
        mixin (nullCheck!`node.nonVoidInitializer = parseNonVoidInitializer()`);
        return node;
    }

    /**
     * Parses StructMemberInitializers
     *
     * $(GRAMMAR $(RULEDEF structMemberInitializers):
     *     $(RULE structMemberInitializer) ($(LITERAL ',') $(RULE structMemberInitializer)?)*
     *     ;)
     */
    StructMemberInitializers parseStructMemberInitializers()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!StructMemberInitializers;
        StructMemberInitializer[] structMemberInitializers;
        do
        {
            auto structMemberInitializer = parseStructMemberInitializer();
            if (structMemberInitializer is null)
            {
                deallocate(node);
                return null;
            }
            else
                structMemberInitializers ~= structMemberInitializer;

            if (currentIs(tok!","))
            {
                advance();
                if (!currentIs(tok!"}"))
                    continue;
                else
                    break;
            }
            else
                break;
        } while (moreTokens());
        node.structMemberInitializers = ownArray(structMemberInitializers);
        return node;
    }

    /**
     * Parses a SwitchStatement
     *
     * $(GRAMMAR $(RULEDEF switchStatement):
     *     $(LITERAL 'switch') $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)') $(RULE statement)
     *     ;)
     */
    SwitchStatement parseSwitchStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!SwitchStatement;
        expect(tok!"switch");
        expect(tok!"(");
        mixin (nullCheck!`node.expression = parseExpression()`);
        expect(tok!")");
        mixin (nullCheck!`node.statement = parseStatement()`);
        return node;
    }

    /**
     * Parses a Symbol
     *
     * $(GRAMMAR $(RULEDEF symbol):
     *     $(LITERAL '.')? $(RULE identifierOrTemplateChain)
     *     ;)
     */
    Symbol parseSymbol()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Symbol;
        if (currentIs(tok!"."))
        {
            node.dot = true;
            advance();
        }
        mixin (nullCheck!`node.identifierOrTemplateChain = parseIdentifierOrTemplateChain()`);
        return node;
    }

    /**
     * Parses a SynchronizedStatement
     *
     * $(GRAMMAR $(RULEDEF synchronizedStatement):
     *     $(LITERAL 'synchronized') ($(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)'))? $(RULE statementNoCaseNoDefault)
     *     ;)
     */
    SynchronizedStatement parseSynchronizedStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!SynchronizedStatement;
        expect(tok!"synchronized");
        if (currentIs(tok!"("))
        {
            expect(tok!"(");
            mixin (nullCheck!`node.expression = parseExpression()`);
            expect(tok!")");
        }
        mixin (nullCheck!`node.statementNoCaseNoDefault = parseStatementNoCaseNoDefault()`);
        return node;
    }

    /**
     * Parses a TemplateAliasParameter
     *
     * $(GRAMMAR $(RULEDEF templateAliasParameter):
     *     $(LITERAL 'alias') $(RULE type)? $(LITERAL Identifier) ($(LITERAL ':') ($(RULE type) | $(RULE assignExpression)))? ($(LITERAL '=') ($(RULE type) | $(RULE assignExpression)))?
     *     ;)
     */
    TemplateAliasParameter parseTemplateAliasParameter()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TemplateAliasParameter;
        expect(tok!"alias");
        if (currentIs(tok!"identifier") && !peekIs(tok!"."))
        {
            if (peekIsOneOf(tok!",", tok!")", tok!"=", tok!":"))
                node.identifier = advance();
            else
                goto type;
        }
        else
        {
    type:
            if ((node.type = parseType()) is null) { deallocate(node); return null; }
            auto ident = expect(tok!"identifier");
            if (ident is null) { deallocate(node); return null; }
            node.identifier = *ident;
        }

        if (currentIs(tok!":"))
        {
            advance();
            if (isType())
                mixin (nullCheck!`node.colonType = parseType()`);
            else
                mixin (nullCheck!`node.colonExpression = parseAssignExpression()`);
        }
        if (currentIs(tok!"="))
        {
            advance();
            if (isType())
                mixin (nullCheck!`node.assignType = parseType()`);
            else
                mixin (nullCheck!`node.assignExpression = parseAssignExpression()`);
        }
        return node;
    }

    /**
     * Parses a TemplateArgument
     *
     * $(GRAMMAR $(RULEDEF templateArgument):
     *       $(RULE type)
     *     | $(RULE assignExpression)
     *     ;)
     */
    TemplateArgument parseTemplateArgument()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        if (suppressedErrorCount > MAX_ERRORS) return null;
        auto node = allocate!TemplateArgument;
        auto b = setBookmark();
        auto t = parseType();
        if (t !is null && currentIsOneOf(tok!",", tok!")"))
        {
            abandonBookmark(b);
            node.type = t;
        }
        else
        {
            goToBookmark(b);
            if ((node.assignExpression = parseAssignExpression()) is null) { deallocate(node); return null; }
        }
        return node;
    }

    /**
     * Parses a TemplateArgumentList
     *
     * $(GRAMMAR $(RULEDEF templateArgumentList):
     *     $(RULE templateArgument) ($(LITERAL ',') $(RULE templateArgument)?)*
     *     ;)
     */
    TemplateArgumentList parseTemplateArgumentList()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseCommaSeparatedRule!(TemplateArgumentList, TemplateArgument)(true);
    }

    /**
     * Parses TemplateArguments
     *
     * $(GRAMMAR $(RULEDEF templateArguments):
     *     $(LITERAL '!') ($(LITERAL '$(LPAREN)') $(RULE templateArgumentList)? $(LITERAL '$(RPAREN)')) | $(RULE templateSingleArgument)
     *     ;)
     */
    TemplateArguments parseTemplateArguments()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        if (suppressedErrorCount > MAX_ERRORS) return null;
        auto node = allocate!TemplateArguments;
        expect(tok!"!");
        if (currentIs(tok!"("))
        {
            advance();
            if (!currentIs(tok!")"))
                mixin (nullCheck!`node.templateArgumentList = parseTemplateArgumentList()`);
            if (expect(tok!")") is null) { deallocate(node); return null; }
        }
        else
            mixin (nullCheck!`node.templateSingleArgument = parseTemplateSingleArgument()`);
        return node;
    }

    /**
     * Parses a TemplateDeclaration
     *
     * $(GRAMMAR $(RULEDEF templateDeclaration):
     *       $(LITERAL 'template') $(LITERAL Identifier) $(RULE templateParameters) $(RULE constraint)? $(LITERAL '{') $(RULE declaration)* $(LITERAL '}')
     *     ;)
     */
    TemplateDeclaration parseTemplateDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TemplateDeclaration;
        node.comment = comment;
        comment = null;
        expect(tok!"template");
        auto ident = expect(tok!"identifier");
        if (ident is null) { deallocate(node); return null; }
        node.name = *ident;
        mixin (nullCheck!`node.templateParameters = parseTemplateParameters()`);
        if (currentIs(tok!"if"))
            mixin (nullCheck!`node.constraint = parseConstraint()`);
        auto start = expect(tok!"{");
        if (start is null) { deallocate(node); return null; } else node.startLocation = start.index;
        Declaration[] declarations;
        while (moreTokens() && !currentIs(tok!"}"))
        {
            auto decl = parseDeclaration();
            if (decl !is null)
                declarations ~= decl;
        }
        node.declarations = ownArray(declarations);
        auto end = expect(tok!"}");
        if (end !is null) node.endLocation = end.index;
        return node;
    }

    /**
     * Parses a TemplateInstance
     *
     * $(GRAMMAR $(RULEDEF templateInstance):
     *     $(LITERAL Identifier) $(RULE templateArguments)
     *     ;)
     */
    TemplateInstance parseTemplateInstance()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        if (suppressedErrorCount > MAX_ERRORS) return null;
        auto node = allocate!TemplateInstance;
        auto ident = expect(tok!"identifier");
        if (ident is null) { deallocate(node); return null; }
        node.identifier = *ident;
        mixin (nullCheck!`node.templateArguments = parseTemplateArguments()`);
        if (node.templateArguments is null)
            return null;
        return node;
    }

    /**
     * Parses a TemplateMixinExpression
     *
     * $(GRAMMAR $(RULEDEF templateMixinExpression):
     *     $(LITERAL 'mixin') $(RULE mixinTemplateName) $(RULE templateArguments)? $(LITERAL Identifier)?
     *     ;)
     */
    TemplateMixinExpression parseTemplateMixinExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TemplateMixinExpression;
        if (expect(tok!"mixin") is null) { deallocate(node); return null; }
        mixin (nullCheck!`node.mixinTemplateName = parseMixinTemplateName()`);
        if (currentIs(tok!"!"))
            mixin (nullCheck!`node.templateArguments = parseTemplateArguments()`);
        if (currentIs(tok!"identifier"))
            node.identifier = advance();
        return node;
    }

    /**
     * Parses a TemplateParameter
     *
     * $(GRAMMAR $(RULEDEF templateParameter):
     *       $(RULE templateTypeParameter)
     *     | $(RULE templateValueParameter)
     *     | $(RULE templateAliasParameter)
     *     | $(RULE templateTupleParameter)
     *     | $(RULE templateThisParameter)
     *     ;)
     */
    TemplateParameter parseTemplateParameter()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TemplateParameter;
        switch (current.type)
        {
        case tok!"alias":
            mixin (nullCheck!`node.templateAliasParameter = parseTemplateAliasParameter()`);
            break;
        case tok!"identifier":
            if (peekIs(tok!"..."))
                mixin (nullCheck!`node.templateTupleParameter = parseTemplateTupleParameter()`);
            else if (peekIsOneOf(tok!":", tok!"=", tok!",", tok!")"))
                mixin (nullCheck!`node.templateTypeParameter = parseTemplateTypeParameter()`);
            else
                mixin (nullCheck!`node.templateValueParameter = parseTemplateValueParameter()`);
            break;
        case tok!"this":
            mixin (nullCheck!`node.templateThisParameter = parseTemplateThisParameter()`);
            break;
        default:
            mixin (nullCheck!`node.templateValueParameter = parseTemplateValueParameter()`);
            break;
        }
        return node;
    }

    /**
     * Parses an TemplateParameterList
     *
     * $(GRAMMAR $(RULEDEF templateParameterList):
     *     $(RULE templateParameter) ($(LITERAL ',') $(RULE templateParameter)?)*
     *     ;)
     */
    TemplateParameterList parseTemplateParameterList()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseCommaSeparatedRule!(TemplateParameterList, TemplateParameter)();
    }

    /**
     * Parses TemplateParameters
     *
     * $(GRAMMAR $(RULEDEF templateParameters):
     *     $(LITERAL '$(LPAREN)') $(RULE templateParameterList)? $(LITERAL '$(RPAREN)')
     *     ;)
     */
    TemplateParameters parseTemplateParameters()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TemplateParameters;
        if (expect(tok!"(") is null) { deallocate(node); return null; }
        if (!currentIs(tok!")"))
            if ((node.templateParameterList = parseTemplateParameterList()) is null) { deallocate(node); return null; }
        if (expect(tok!")") is null) { deallocate(node); return null; }
        return node;
    }

    /**
     * Parses a TemplateSingleArgument
     *
     * $(GRAMMAR $(RULEDEF templateSingleArgument):
     *       $(RULE builtinType)
     *     | $(LITERAL Identifier)
     *     | $(LITERAL CharacterLiteral)
     *     | $(LITERAL StringLiteral)
     *     | $(LITERAL IntegerLiteral)
     *     | $(LITERAL FloatLiteral)
     *     | $(LITERAL '_true')
     *     | $(LITERAL '_false')
     *     | $(LITERAL '_null')
     *     | $(LITERAL 'this')
     *     | $(LITERAL '___DATE__')
     *     | $(LITERAL '___TIME__')
     *     | $(LITERAL '___TIMESTAMP__')
     *     | $(LITERAL '___VENDOR__')
     *     | $(LITERAL '___VERSION__')
     *     | $(LITERAL '___FILE__')
     *     | $(LITERAL '___LINE__')
     *     | $(LITERAL '___MODULE__')
     *     | $(LITERAL '___FUNCTION__')
     *     | $(LITERAL '___PRETTY_FUNCTION__')
     *     ;)
     */
    TemplateSingleArgument parseTemplateSingleArgument()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TemplateSingleArgument;
        if (!moreTokens)
        {
            error("template argument expected instead of EOF");
            return null;
        }
        switch (current.type)
        {
        case tok!"true":
        case tok!"false":
        case tok!"null":
        case tok!"this":
        case tok!"identifier":
        mixin (SPECIAL_CASES);
        mixin (LITERAL_CASES);
        mixin (BUILTIN_TYPE_CASES);
            node.token = advance();
            break;
        default:
            error(`Invalid template argument. (Try enclosing in parenthesis?)`);
            return null;
        }
        return node;
    }

    /**
     * Parses a TemplateThisParameter
     *
     * $(GRAMMAR $(RULEDEF templateThisParameter):
     *     $(LITERAL 'this') $(RULE templateTypeParameter)
     *     ;)
     */
    TemplateThisParameter parseTemplateThisParameter()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TemplateThisParameter;
        expect(tok!"this");
        mixin (nullCheck!`node.templateTypeParameter = parseTemplateTypeParameter()`);
        return node;
    }

    /**
     * Parses an TemplateTupleParameter
     *
     * $(GRAMMAR $(RULEDEF templateTupleParameter):
     *     $(LITERAL Identifier) $(LITERAL '...')
     *     ;)
     */
    TemplateTupleParameter parseTemplateTupleParameter()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TemplateTupleParameter;
        auto i = expect(tok!"identifier");
        if (i is null)
            return null;
        node.identifier = *i;
        if (expect(tok!"...") is null) { deallocate(node); return null; }
        return node;
    }

    /**
     * Parses a TemplateTypeParameter
     *
     * $(GRAMMAR $(RULEDEF templateTypeParameter):
     *     $(LITERAL Identifier) ($(LITERAL ':') $(RULE type))? ($(LITERAL '=') $(RULE type))?
     *     ;)
     */
    TemplateTypeParameter parseTemplateTypeParameter()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TemplateTypeParameter;
        auto ident = expect(tok!"identifier");
        if (ident is null) { deallocate(node); return null; }
        node.identifier = *ident;
        if (currentIs(tok!":"))
        {
            advance();
            mixin (nullCheck!`node.colonType = parseType()`);
        }
        if (currentIs(tok!"="))
        {
            advance();
            mixin (nullCheck!`node.assignType = parseType()`);
        }
        return node;
    }

    /**
     * Parses a TemplateValueParameter
     *
     * $(GRAMMAR $(RULEDEF templateValueParameter):
     *     $(RULE type) $(LITERAL Identifier) ($(LITERAL ':') $(RULE assignExpression))? $(RULE templateValueParameterDefault)?
     *     ;)
     */
    TemplateValueParameter parseTemplateValueParameter()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TemplateValueParameter;
        if ((node.type = parseType()) is null) { deallocate(node); return null; }
        auto ident = expect(tok!"identifier");
        if (ident is null) { deallocate(node); return null; }
        node.identifier = *ident;
        if (currentIs(tok!":"))
        {
            advance();
            if ((node.assignExpression = parseAssignExpression()) is null) { deallocate(node); return null; }
        }
        if (currentIs(tok!"="))
        {
            if ((node.templateValueParameterDefault = parseTemplateValueParameterDefault()) is null)
                return null;
        }
        return node;
    }

    /**
     * Parses a TemplateValueParameterDefault
     *
     * $(GRAMMAR $(RULEDEF templateValueParameterDefault):
     *     $(LITERAL '=') ($(LITERAL '___FILE__') | $(LITERAL '___MODULE__') | $(LITERAL '___LINE__') | $(LITERAL '___FUNCTION__') | $(LITERAL '___PRETTY_FUNCTION__') | $(RULE assignExpression))
     *     ;)
     */
    TemplateValueParameterDefault parseTemplateValueParameterDefault()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TemplateValueParameterDefault;
        expect(tok!"=");
        switch (current.type)
        {
        case tok!"__FILE__":
        case tok!"__MODULE__":
        case tok!"__LINE__":
        case tok!"__FUNCTION__":
        case tok!"__PRETTY_FUNCTION__":
            node.token = advance();
            break;
        default:
            mixin (nullCheck!`node.assignExpression = parseAssignExpression()`);
            break;
        }
        return node;
    }

    /**
     * Parses a TernaryExpression
     *
     * $(GRAMMAR $(RULEDEF ternaryExpression):
     *     $(RULE orOrExpression) ($(LITERAL '?') $(RULE expression) $(LITERAL ':') $(RULE ternaryExpression))?
     *     ;)
     */
    ExpressionNode parseTernaryExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));

        auto orOrExpression = parseOrOrExpression();
        if (orOrExpression is null)
            return null;
        if (currentIs(tok!"?"))
        {
            TernaryExpression node = allocate!TernaryExpression;
            node.orOrExpression = orOrExpression;
            advance();
            mixin (nullCheck!`node.expression = parseExpression()`);
            auto colon = expect(tok!":");
            if (colon is null) { deallocate(node); return null; }
            node.colon = *colon;
            mixin (nullCheck!`node.ternaryExpression = parseTernaryExpression()`);
            return node;
        }
        return orOrExpression;
    }

    /**
     * Parses a ThrowStatement
     *
     * $(GRAMMAR $(RULEDEF throwStatement):
     *     $(LITERAL 'throw') $(RULE expression) $(LITERAL ';')
     *     ;)
     */
    ThrowStatement parseThrowStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!ThrowStatement;
        expect(tok!"throw");
        mixin (nullCheck!`node.expression = parseExpression()`);
        expect(tok!";");
        return node;
    }

    /**
     * Parses an TraitsExpression
     *
     * $(GRAMMAR $(RULEDEF traitsExpression):
     *     $(LITERAL '___traits') $(LITERAL '$(LPAREN)') $(LITERAL Identifier) $(LITERAL ',') $(RULE TemplateArgumentList) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    TraitsExpression parseTraitsExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TraitsExpression;
        if (expect(tok!"__traits") is null) { deallocate(node); return null; }
        if (expect(tok!"(") is null) { deallocate(node); return null; }
        auto ident = expect(tok!"identifier");
        if (ident is null) { deallocate(node); return null; }
        node.identifier = *ident;
        if (currentIs(tok!","))
        {
            advance();
            mixin (nullCheck!`(node.templateArgumentList = parseTemplateArgumentList())`);
        }
        mixin (nullCheck!`expect(tok!")")`);
        return node;
    }

    /**
     * Parses a TryStatement
     *
     * $(GRAMMAR $(RULEDEF tryStatement):
     *     $(LITERAL 'try') $(RULE declarationOrStatement) ($(RULE catches) | $(RULE catches) $(RULE finally) | $(RULE finally))
     *     ;)
     */
    TryStatement parseTryStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TryStatement;
        expect(tok!"try");
        mixin (nullCheck!`node.declarationOrStatement = parseDeclarationOrStatement()`);
        if (currentIs(tok!"catch"))
            mixin (nullCheck!`node.catches = parseCatches()`);
        if (currentIs(tok!"finally"))
            mixin (nullCheck!`node.finally_ = parseFinally()`);
        return node;
    }

    /**
     * Parses a Type
     *
     * $(GRAMMAR $(RULEDEF type):
     *     $(RULE typeConstructors)? $(RULE type2) $(RULE typeSuffix)*
     *     ;)
     */
    Type parseType()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Type;
        if (!moreTokens)
        {
            error("type expected");
            return null;
        }
        switch (current.type)
        {
        case tok!"const":
        case tok!"immutable":
        case tok!"inout":
        case tok!"shared":
            if (!peekIs(tok!"("))
                mixin (nullCheck!`node.typeConstructors = parseTypeConstructors()`);
            break;
        default:
            break;
        }
        mixin (nullCheck!`node.type2 = parseType2()`);
        if (node.type2 is null)
            return null;
        TypeSuffix[] typeSuffixes;
        loop: while (moreTokens()) switch (current.type)
        {
        case tok!"[":
            // Allow this to fail because of the madness that is the
            // newExpression rule. Something starting with '[' may be arguments
            // to the newExpression instead of part of the type
            auto newBookmark = setBookmark();
            auto suffix = parseTypeSuffix();
            if (suffix !is null)
            {
                abandonBookmark(newBookmark);
                typeSuffixes ~= suffix;
            }
            else
            {
                goToBookmark(newBookmark);
                break loop;
            }
            break;
        case tok!"*":
        case tok!"delegate":
        case tok!"function":
            auto suffix = parseTypeSuffix();
            if (suffix !is null)
                typeSuffixes ~= suffix;
            else
                return null;
            break;
        default:
            break loop;
        }
        node.typeSuffixes = ownArray(typeSuffixes);
        return node;
    }

    /**
     * Parses a Type2
     *
     * $(GRAMMAR $(RULEDEF type2):
     *       $(RULE builtinType)
     *     | $(RULE symbol)
     *     | $(RULE typeofExpression) ($(LITERAL '.') $(RULE identifierOrTemplateChain))?
     *     | $(RULE typeConstructor) $(LITERAL '$(LPAREN)') $(RULE type) $(LITERAL '$(RPAREN)')
     *     | $(RULE vector)
     *     ;)
     */
    Type2 parseType2()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!Type2;
        if (!moreTokens)
        {
            error("type2 expected instead of EOF");
            return null;
        }
        switch (current.type)
        {
        case tok!"identifier":
        case tok!".":
            if ((node.symbol = parseSymbol()) is null)
                return null;
            break;
        mixin (BUILTIN_TYPE_CASES);
            if ((node.builtinType = parseBuiltinType()) == tok!"")
                return null;
            break;
        case tok!"typeof":
            if ((node.typeofExpression = parseTypeofExpression()) is null)
                return null;
            if (currentIs(tok!"."))
            {
                advance();
                mixin (nullCheck!`node.identifierOrTemplateChain = parseIdentifierOrTemplateChain()`);
                if (node.identifierOrTemplateChain is null)
                    return null;
            }
            break;
        case tok!"const":
        case tok!"immutable":
        case tok!"inout":
        case tok!"shared":
            node.typeConstructor = advance().type;
            mixin (nullCheck!`expect(tok!"(")`);
            mixin (nullCheck!`(node.type = parseType())`);
            mixin (nullCheck!`expect(tok!")")`);
            break;
        case tok!"__vector":
            if ((node.vector = parseVector()) is null)
                return null;
            break;
        default:
            error("Basic type, type constructor, symbol, or typeof expected");
            return null;
        }
        return node;
    }

    /**
     * Parses a TypeConstructor
     *
     * $(GRAMMAR $(RULEDEF typeConstructor):
     *       $(LITERAL 'const')
     *     | $(LITERAL 'immutable')
     *     | $(LITERAL 'inout')
     *     | $(LITERAL 'shared')
     *     ;)
     */
    IdType parseTypeConstructor(bool validate = true)
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        switch (current.type)
        {
        case tok!"const":
        case tok!"immutable":
        case tok!"inout":
        case tok!"shared":
            if (!peekIs(tok!"("))
                return advance().type;
            goto default;
        default:
            if (validate)
                error(`"const", "immutable", "inout", or "shared" expected`);
            return tok!"";
        }
    }

    /**
     * Parses TypeConstructors
     *
     * $(GRAMMAR $(RULEDEF typeConstructors):
     *     $(RULE typeConstructor)+
     *     ;)
     */
    IdType[] parseTypeConstructors()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        IdType[] r;
        while (moreTokens())
        {
            IdType type = parseTypeConstructor(false);
            if (type == tok!"")
                break;
            else
                r ~= type;
        }
        return r;
    }

    /**
     * Parses a TypeSpecialization
     *
     * $(GRAMMAR $(RULEDEF typeSpecialization):
     *       $(RULE type)
     *     | $(LITERAL 'struct')
     *     | $(LITERAL 'union')
     *     | $(LITERAL 'class')
     *     | $(LITERAL 'interface')
     *     | $(LITERAL 'enum')
     *     | $(LITERAL 'function')
     *     | $(LITERAL 'delegate')
     *     | $(LITERAL 'super')
     *     | $(LITERAL 'const')
     *     | $(LITERAL 'immutable')
     *     | $(LITERAL 'inout')
     *     | $(LITERAL 'shared')
     *     | $(LITERAL 'return')
     *     | $(LITERAL 'typedef')
     *     | $(LITERAL '___parameters')
     *     ;)
     */
    TypeSpecialization parseTypeSpecialization()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TypeSpecialization;
        switch (current.type)
        {
        case tok!"struct":
        case tok!"union":
        case tok!"class":
        case tok!"interface":
        case tok!"enum":
        case tok!"function":
        case tok!"delegate":
        case tok!"super":
        case tok!"return":
        case tok!"typedef":
        case tok!"__parameters":
        case tok!"const":
        case tok!"immutable":
        case tok!"inout":
        case tok!"shared":
            if (peekIsOneOf(tok!")", tok!","))
            {
                node.token = advance();
                break;
            }
            goto default;
        default:
            mixin (nullCheck!`node.type = parseType()`);
            break;
        }
        return node;
    }

    /**
     * Parses a TypeSuffix
     *
     * $(GRAMMAR $(RULEDEF typeSuffix):
     *       $(LITERAL '*')
     *     | $(LITERAL '[') $(RULE type)? $(LITERAL ']')
     *     | $(LITERAL '[') $(RULE assignExpression) $(LITERAL ']')
     *     | $(LITERAL '[') $(RULE assignExpression) $(LITERAL '..')  $(RULE assignExpression) $(LITERAL ']')
     *     | ($(LITERAL 'delegate') | $(LITERAL 'function')) $(RULE parameters) $(RULE memberFunctionAttribute)*
     *     ;)
     */
    TypeSuffix parseTypeSuffix()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TypeSuffix;
        switch (current.type)
        {
        case tok!"*":
            node.star = advance();
            return node;
        case tok!"[":
            node.array = true;
            advance();
            if (currentIs(tok!"]"))
            {
                advance();
                return node;
            }
            auto bookmark = setBookmark();
            auto type = parseType();
            if (type !is null && currentIs(tok!"]"))
            {
                abandonBookmark(bookmark);
                node.type = type;
            }
            else
            {
                goToBookmark(bookmark);
                mixin (nullCheck!`node.low = parseAssignExpression()`);
                mixin (nullCheck!`node.low`);
                if (currentIs(tok!".."))
                {
                    advance();
                    mixin (nullCheck!`node.high = parseAssignExpression()`);
                    mixin (nullCheck!`node.high`);
                }
            }
            mixin (nullCheck!`expect(tok!"]")`);
            return node;
        case tok!"delegate":
        case tok!"function":
            node.delegateOrFunction = advance();
            mixin (nullCheck!`node.parameters = parseParameters()`);
            MemberFunctionAttribute[] memberFunctionAttributes;
            while (currentIsMemberFunctionAttribute())
                memberFunctionAttributes ~= parseMemberFunctionAttribute();
            node.memberFunctionAttributes = ownArray(memberFunctionAttributes);
            return node;
        default:
            error(`"*", "[", "delegate", or "function" expected.`);
            return null;
        }
    }

    /**
     * Parses a TypeidExpression
     *
     * $(GRAMMAR $(RULEDEF typeidExpression):
     *     $(LITERAL 'typeid') $(LITERAL '$(LPAREN)') ($(RULE type) | $(RULE expression)) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    TypeidExpression parseTypeidExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TypeidExpression;
        expect(tok!"typeid");
        expect(tok!"(");
        auto b = setBookmark();
        auto t = parseType();
        if (t is null || !currentIs(tok!")"))
        {
            goToBookmark(b);
            mixin (nullCheck!`node.expression = parseExpression()`);
            mixin (nullCheck!`node.expression`);
        }
        else
        {
            abandonBookmark(b);
            node.type = t;
        }
        expect(tok!")");
        return node;
    }

    /**
     * Parses a TypeofExpression
     *
     * $(GRAMMAR $(RULEDEF typeofExpression):
     *     $(LITERAL 'typeof') $(LITERAL '$(LPAREN)') ($(RULE expression) | $(LITERAL 'return')) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    TypeofExpression parseTypeofExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!TypeofExpression;
        expect(tok!"typeof");
        expect(tok!"(");
        if (currentIs(tok!"return"))
            node.return_ = advance();
        else
            mixin (nullCheck!`node.expression = parseExpression()`);
        expect(tok!")");
        return node;
    }

    /**
     * Parses a UnaryExpression
     *
     * $(GRAMMAR $(RULEDEF unaryExpression):
     *       $(RULE primaryExpression)
     *     | $(LITERAL '&') $(RULE unaryExpression)
     *     | $(LITERAL '!') $(RULE unaryExpression)
     *     | $(LITERAL '*') $(RULE unaryExpression)
     *     | $(LITERAL '+') $(RULE unaryExpression)
     *     | $(LITERAL '-') $(RULE unaryExpression)
     *     | $(LITERAL '~') $(RULE unaryExpression)
     *     | $(LITERAL '++') $(RULE unaryExpression)
     *     | $(LITERAL '--') $(RULE unaryExpression)
     *     | $(RULE newExpression)
     *     | $(RULE deleteExpression)
     *     | $(RULE castExpression)
     *     | $(RULE assertExpression)
     *     | $(RULE functionCallExpression)
     *     | $(RULE sliceExpression)
     *     | $(RULE indexExpression)
     *     | $(LITERAL '$(LPAREN)') $(RULE type) $(LITERAL '$(RPAREN)') $(LITERAL '.') $(RULE identifierOrTemplateInstance)
     *     | $(RULE unaryExpression) $(LITERAL '.') $(RULE identifierOrTemplateInstance)
     *     | $(RULE unaryExpression) $(LITERAL '--')
     *     | $(RULE unaryExpression) $(LITERAL '++')
     *     ;)
     */
    UnaryExpression parseUnaryExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        if (!moreTokens())
            return null;
        auto node = allocate!UnaryExpression;
        switch (current.type)
        {
        case tok!"const":
        case tok!"immutable":
        case tok!"inout":
        case tok!"shared":
            auto b = setBookmark();
            if (peekIs(tok!"("))
            {
                advance();
                auto past = peekPastParens();
                if (past !is null && past.type == tok!".")
                {
                    goToBookmark(b);
                    goto default;
                }
            }
            goToBookmark(b);
            goto case;
        case tok!"scope":
        case tok!"pure":
        case tok!"nothrow":
            if ((node.functionCallExpression = parseFunctionCallExpression()) is null)
            {
                deallocate(node);
                return null;
            }
            break;
        case tok!"&":
        case tok!"!":
        case tok!"*":
        case tok!"+":
        case tok!"-":
        case tok!"~":
        case tok!"++":
        case tok!"--":
            node.prefix = advance();
            mixin (nullCheck!`node.unaryExpression = parseUnaryExpression()`);
            break;
        case tok!"new":
            mixin (nullCheck!`node.newExpression = parseNewExpression()`);
            break;
        case tok!"delete":
            mixin (nullCheck!`node.deleteExpression = parseDeleteExpression()`);
            break;
        case tok!"cast":
            mixin (nullCheck!`node.castExpression = parseCastExpression()`);
            break;
        case tok!"assert":
            mixin (nullCheck!`node.assertExpression = parseAssertExpression()`);
            break;
        case tok!"(":
            // handle (type).identifier
            auto b = setBookmark();
            skipParens();
            if (startsWith(tok!".", tok!"identifier"))
            {
                // go back to the (
                goToBookmark(b);
                b = setBookmark();
                advance();
                auto t = parseType();
                if (t is null || !currentIs(tok!")"))
                {
                    goToBookmark(b);
                    goto default;
                }
                abandonBookmark(b);
                node.type = t;
                advance(); // )
                advance(); // .
                mixin (nullCheck!`node.identifierOrTemplateInstance = parseIdentifierOrTemplateInstance()`);
                break;
            }
            else
            {
                // not (type).identifier, so treat as primary expression
                goToBookmark(b);
                goto default;
            }
        default:
            if ((node.primaryExpression = parsePrimaryExpression()) is null)
            {
                deallocate(node);
                return null;
            }
            break;
        }

        loop: while (moreTokens()) switch (current.type)
        {
        case tok!"!":
            if (peekIs(tok!"("))
            {
                index++;
                auto p = peekPastParens();
                bool jump =  (currentIs(tok!"(") && p !is null && p.type == tok!"(")
                    || peekIs(tok!"(");
                index--;
                if (jump)
                    goto case tok!"(";
                else
                    break loop;
            }
            else
                break loop;
        case tok!"(":
            auto newUnary = allocate!UnaryExpression();
            newUnary.functionCallExpression = parseFunctionCallExpression(node);
            node = newUnary;
            break;
        case tok!"++":
        case tok!"--":
            auto n = allocate!UnaryExpression();
            n.unaryExpression = node;
            n.suffix = advance();
            node = n;
            break;
        case tok!"[":
            auto n = allocate!UnaryExpression;
            if (isSliceExpression())
                n.sliceExpression = parseSliceExpression(node);
            else
                n.indexExpression = parseIndexExpression(node);
            node = n;
            break;
        case tok!".":
            advance();
            auto n = allocate!UnaryExpression();
            n.unaryExpression = node;
            n.identifierOrTemplateInstance = parseIdentifierOrTemplateInstance();
            node = n;
            break;
        default:
            break loop;
        }
        return node;
    }

    /**
     * Parses an UnionDeclaration
     *
     * $(GRAMMAR $(RULEDEF unionDeclaration):
     *       $(LITERAL 'union') $(LITERAL Identifier) $(RULE templateParameters) $(RULE constraint)? $(RULE structBody)
     *     | $(LITERAL 'union') $(LITERAL Identifier) ($(RULE structBody) | $(LITERAL ';'))
     *     | $(LITERAL 'union') $(RULE structBody)
     *     ;)
     */
    UnionDeclaration parseUnionDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!UnionDeclaration;
        // grab line number even if it's anonymous
        auto t = expect(tok!"union");
        if (currentIs(tok!"identifier"))
        {
            node.name = advance();
            if (currentIs(tok!"("))
            {
                mixin (nullCheck!`node.templateParameters = parseTemplateParameters()`);
                if (currentIs(tok!"if"))
                    mixin (nullCheck!`node.constraint = parseConstraint()`);
                mixin (nullCheck!`node.structBody = parseStructBody()`);
            }
            else
                goto semiOrStructBody;
        }
        else
        {
            node.name.line = t.line;
            node.name.column = t.column;
    semiOrStructBody:
            if (currentIs(tok!";"))
                advance();
            else
                mixin (nullCheck!`node.structBody = parseStructBody()`);
        }
        return node;
    }

    /**
     * Parses a Unittest
     *
     * $(GRAMMAR $(RULEDEF unittest):
     *     $(LITERAL 'unittest') $(RULE blockStatement)
     *     ;)
     */
    Unittest parseUnittest()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        mixin (simpleParse!(Unittest, tok!"unittest", "blockStatement|parseBlockStatement"));
    }

    /**
     * Parses a VariableDeclaration
     *
     * $(GRAMMAR $(RULEDEF variableDeclaration):
     *       $(RULE storageClass)* $(RULE type) $(RULE declarator) ($(LITERAL ',') $(RULE declarator))* $(LITERAL ';')
     *     | $(RULE storageClass)* $(RULE type) $(LITERAL identifier) $(LITERAL '=') $(RULE functionBody) $(LITERAL ';')
     *     | $(RULE autoDeclaration)
     *     ;)
     */
    VariableDeclaration parseVariableDeclaration(Type type = null, bool isAuto = false,
        Attribute[] attributes = null)
    {
        mixin (traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!VariableDeclaration;
        node.attributes = attributes;
        if (isAuto)
        {
            mixin (nullCheck!`node.autoDeclaration = parseAutoDeclaration()`);
            if (node.autoDeclaration !is null)
            {
                node.comment = node.autoDeclaration.comment;
                return node;
            }
            else
            {
                deallocate(node);
                return null;
            }
        }

        StorageClass[] storageClasses;
        while (isStorageClass())
        {
            auto s = parseStorageClass();
            if (s !is null)
                storageClasses ~= s;
            else
            {
                deallocate(node);
                return null;
            }
        }
        node.storageClasses = ownArray(storageClasses);

        node.type = type is null ? parseType() : type;
        node.comment = comment;

        // TODO: handle function bodies correctly

        Declarator[] declarators;
        while (true)
        {
            auto declarator = parseDeclarator();
            mixin (nullCheck!`declarator`);
            declarators ~= declarator;
            if (moreTokens() && currentIs(tok!","))
            {
                declarator.comment = current.trailingComment;
                advance();
            }
            else
                break;
        }
        node.declarators = ownArray(declarators);
        auto semicolon = expect(tok!";");
        mixin (nullCheck!`semicolon`);
        declarators[$ - 1].comment = semicolon.trailingComment;
        return node;
    }

    /**
     * Parses a Vector
     *
     * $(GRAMMAR $(RULEDEF vector):
     *     $(LITERAL '___vector') $(LITERAL '$(LPAREN)') $(RULE type) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    Vector parseVector()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        mixin (simpleParse!(Vector, tok!"__vector", tok!"(", "type|parseType", tok!")"));
    }

    /**
     * Parses a VersionCondition
     *
     * $(GRAMMAR $(RULEDEF versionCondition):
     *     $(LITERAL 'version') $(LITERAL '$(LPAREN)') ($(LITERAL IntegerLiteral) | $(LITERAL Identifier) | $(LITERAL 'unittest') | $(LITERAL 'assert')) $(LITERAL '$(RPAREN)')
     *     ;)
     */
    VersionCondition parseVersionCondition()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!VersionCondition;
        mixin (expectSequence!(tok!"version", tok!"("));
        if (currentIsOneOf(tok!"intLiteral", tok!"identifier",
            tok!"unittest", tok!"assert"))
        {
            node.token = advance();
        }
        else
        {
            error(`Expected an integer literal, an identifier, "assert", or "unittest"`);
            return null;
        }
        expect(tok!")");
        return node;
    }

    /**
     * Parses a VersionSpecification
     *
     * $(GRAMMAR $(RULEDEF versionSpecification):
     *     $(LITERAL 'version') $(LITERAL '=') ($(LITERAL Identifier) | $(LITERAL IntegerLiteral)) $(LITERAL ';')
     *     ;)
     */
    VersionSpecification parseVersionSpecification()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!VersionSpecification;
        mixin (expectSequence!(tok!"version", tok!"="));
        if (!currentIsOneOf(tok!"identifier", tok!"intLiteral"))
        {
            error("Identifier or integer literal expected");
            return null;
        }
        node.token = advance();
        expect(tok!";");
        return node;
    }

    /**
     * Parses a WhileStatement
     *
     * $(GRAMMAR $(RULEDEF whileStatement):
     *     $(LITERAL 'while') $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)') $(RULE declarationOrStatement)
     *     ;)
     */
    WhileStatement parseWhileStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto node = allocate!WhileStatement;
        expect(tok!"while");
        node.startIndex = current().index;
        expect(tok!"(");
        mixin (nullCheck!`node.expression = parseExpression()`);
        expect(tok!")");
        if (currentIs(tok!"}"))
        {
            error("Statement expected", false);
            return node; // this line makes DCD better
        }
        mixin (nullCheck!`node.declarationOrStatement = parseDeclarationOrStatement()`);
        return node;
    }

    /**
     * Parses a WithStatement
     *
     * $(GRAMMAR $(RULEDEF withStatement):
     *     $(LITERAL 'with') $(LITERAL '$(LPAREN)') $(RULE expression) $(LITERAL '$(RPAREN)') $(RULE statementNoCaseNoDefault)
     *     ;)
     */
    WithStatement parseWithStatement()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        mixin (simpleParse!(WithStatement, tok!"with", tok!"(",
            "expression|parseExpression", tok!")",
            "statementNoCaseNoDefault|parseStatementNoCaseNoDefault"));
    }

    /**
     * Parses an XorExpression
     *
     * $(GRAMMAR $(RULEDEF xorExpression):
     *       $(RULE andExpression)
     *     | $(RULE xorExpression) $(LITERAL '^') $(RULE andExpression)
     *     ;)
     */
    ExpressionNode parseXorExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        return parseLeftAssocBinaryExpression!(XorExpression, AndExpression,
            tok!"^")();
    }

    /**
     * Current error count
     */
    uint errorCount;

    /**
     * Current warning count
     */
    uint warningCount;

    /**
     * Name used when reporting warnings and errors
     */
    string fileName;

    /**
     * Allocator used for creating AST nodes
     */
    CAllocator allocator;

    /**
     * Function that is called when a warning or error is encountered.
     * The parameters are the file name, line number, column number,
     * and the error or warning message.
     */
    void function(string, size_t, size_t, string, bool) messageFunction;

    bool isSliceExpression()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        if (startsWith(tok!"[", tok!"]"))
            return true;
        return hasMagicDelimiter!(tok!"[", tok!"..")();
    }

    void setTokens(const(Token)[] tokens)
    {
        this.tokens = tokens;
    }

    /**
     * Returns: true if there are more tokens
     */
    bool moreTokens() const nothrow pure @safe
    {
        return index < tokens.length;
    }

protected:

    uint suppressedErrorCount;

    enum MAX_ERRORS = 500;

    T[] ownArray(T)(T[] from)
    {
        if (allocator is null)
            return from;
        T[] to = cast(T[]) allocator.allocate(T.sizeof * from.length);
        assert (to.length == from.length, format("from.length = %d, to.length = %d", from.length, to.length));
        to[] = from[];
        return to;
    }

    T allocate(T, Args...)(auto ref Args args)
    {
        import std.stdio;
        if (allocator is null)
            return new T(args);
        enum numBytes = __traits(classInstanceSize, T);
        void[] mem = allocator.allocate(numBytes);
        assert (mem.length == numBytes, format("%d", mem.length));
        T t = emplace!T(mem, args);
        assert (cast(void*) t == mem.ptr, "%x, %x".format(cast(void*) t, mem.ptr));
        return t;
    }

    void deallocate(T)(T t)
    {
        if (allocator !is null)
            allocator.deallocate((cast (void*) t)[0 .. __traits(classInstanceSize, T)]);
    }

    bool isCastQualifier() const
    {
        switch (current.type)
        {
        case tok!"const":
            if (peekIs(tok!")")) return true;
            return startsWith(tok!"const", tok!"shared", tok!")");
        case tok!"immutable":
            return peekIs(tok!")");
        case tok!"inout":
            if (peekIs(tok!")")) return true;
            return startsWith(tok!"inout", tok!"shared", tok!")");
        case tok!"shared":
            if (peekIs(tok!")")) return true;
            if (startsWith(tok!"shared", tok!"const", tok!")")) return true;
            return startsWith(tok!"shared", tok!"inout", tok!")");
        default:
            return false;
        }
    }

    bool isAssociativeArrayLiteral()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        static bool[typeof(current.index)] cached;
        if (auto p = current.index in cached)
            return *p;
        size_t currentIndex = current.index;
        auto b = setBookmark();
        scope(exit) goToBookmark(b);
        advance();
        bool result = !currentIs(tok!"]") && parseExpression() !is null && currentIs(tok!":");
        cached[currentIndex] = result;
        return result;
    }

    bool hasMagicDelimiter(alias L, alias T)()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        auto i = index;
        scope(exit) index = i;
        assert (currentIs(L));
        advance();
        while (moreTokens()) switch (current.type)
        {
        case tok!"{": skipBraces(); break;
        case tok!"(": skipParens(); break;
        case tok!"[": skipBrackets(); break;
        case tok!"]": case tok!"}": return false;
        case T: return true;
        default: advance(); break;
        }
        return false;
    }

    bool isDeclaration()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        if (!moreTokens()) return false;
        switch (current.type)
        {
        case tok!"final":
            return !peekIs(tok!"switch");
        case tok!"debug":
            if (peekIs(tok!":"))
                return true;
            goto case;
        case tok!"version":
            if (peekIs(tok!"="))
                return true;
            if (peekIs(tok!"("))
            {
                auto b = setBookmark();
                scope (exit) goToBookmark(b);
                auto dec = parseDeclaration(true);
                if (dec is null)
                    return false;
                else
                {
                    deallocate(dec);
                    return true;
                }
            }
            return false;
        case tok!"synchronized":
            if (peekIs(tok!"("))
                return false;
            else
                goto default;
        case tok!"static":
            if (peekIs(tok!"if"))
                return false;
            goto case;
        case tok!"scope":
            if (peekIs(tok!"("))
                return false;
            goto case;
        case tok!"const":
        case tok!"immutable":
        case tok!"inout":
        case tok!"shared":
            goto default;
        case tok!"@":
        case tok!"abstract":
        case tok!"alias":
        case tok!"align":
        case tok!"auto":
        case tok!"class":
        case tok!"deprecated":
        case tok!"enum":
        case tok!"export":
        case tok!"extern":
        case tok!"__gshared":
        case tok!"interface":
        case tok!"nothrow":
        case tok!"override":
        case tok!"package":
        case tok!"private":
        case tok!"protected":
        case tok!"public":
        case tok!"pure":
        case tok!"ref":
        case tok!"struct":
        case tok!"union":
        case tok!"unittest":
            return true;
        mixin(BUILTIN_TYPE_CASES);
            return !peekIsOneOf(tok!".", tok!"(");
        case tok!"asm":
        case tok!"break":
        case tok!"case":
        case tok!"continue":
        case tok!"default":
        case tok!"do":
        case tok!"for":
        case tok!"foreach":
        case tok!"foreach_reverse":
        case tok!"goto":
        case tok!"if":
        case tok!"return":
        case tok!"switch":
        case tok!"throw":
        case tok!"try":
        case tok!"while":
        case tok!"{":
        case tok!"assert":
            return false;
        default:
            auto b = setBookmark();
            auto p = parseDeclaration();
            if (p is null)
            {
                goToBookmark(b);
                return false;
            }
            else
            {
                deallocate(p);
                goToBookmark(b);
                return true;
            }
        }
    }

    /// Only use this in template parameter parsing
    bool isType()
    {
        if (!moreTokens()) return false;
        auto b = setBookmark();
        scope (exit) goToBookmark(b);
        auto t = parseType();
        if (t is null) return false; else deallocate(t);
        if (currentIsOneOf(tok!",", tok!")", tok!"=")) return true;
        return false;
    }

    bool isStorageClass()
    {
        if (!moreTokens()) return false;
        switch (current.type)
        {
        case tok!"const":
        case tok!"immutable":
        case tok!"inout":
        case tok!"shared":
            return !peekIs(tok!"(");
        case tok!"@":
        case tok!"deprecated":
        case tok!"abstract":
        case tok!"align":
        case tok!"auto":
        case tok!"enum":
        case tok!"extern":
        case tok!"final":
        case tok!"nothrow":
        case tok!"override":
        case tok!"pure":
        case tok!"ref":
        case tok!"__gshared":
        case tok!"scope":
        case tok!"static":
        case tok!"synchronized":
            return true;
        default:
            return false;
        }
    }

    bool isAttribute()
    {
        if (!moreTokens()) return false;
        switch (current.type)
        {
        case tok!"const":
        case tok!"immutable":
        case tok!"inout":
        case tok!"scope":
            return !peekIs(tok!"(");
        case tok!"static":
            return !peekIsOneOf(tok!"assert", tok!"this", tok!"if", tok!"~");
        case tok!"shared":
            return !(startsWith(tok!"shared", tok!"static", tok!"this")
                || startsWith(tok!"shared", tok!"static", tok!"~")
                || peekIs(tok!"("));
        case tok!"pragma":
            auto b = setBookmark();
            scope(exit) goToBookmark(b);
            advance();
            auto past = peekPastParens();
            if (past is null || *past == tok!";")
                return false;
            return true;
        case tok!"deprecated":
        case tok!"private":
        case tok!"package":
        case tok!"protected":
        case tok!"public":
        case tok!"export":
        case tok!"final":
        case tok!"synchronized":
        case tok!"override":
        case tok!"abstract":
        case tok!"auto":
        case tok!"__gshared":
        case tok!"pure":
        case tok!"nothrow":
        case tok!"@":
        case tok!"ref":
        case tok!"extern":
        case tok!"align":
            return true;
        default:
            return false;
        }
    }

    bool currentIsMemberFunctionAttribute() const
    {
        if (!moreTokens) return false;
        switch (current.type)
        {
        case tok!"const":
        case tok!"immutable":
        case tok!"inout":
        case tok!"shared":
        case tok!"@":
        case tok!"pure":
        case tok!"nothrow":
        case tok!"return":
            return true;
        default:
            return false;
        }
    }

    ExpressionNode parseLeftAssocBinaryExpression(alias ExpressionType,
        alias ExpressionPartType, Operators ...)(ExpressionNode part = null)
    {
        ExpressionNode node;
        mixin ("node = part is null ? parse" ~ ExpressionPartType.stringof ~ "() : part;");
        if (node is null)
        {
            deallocate(node);
            return null;
        }
        while (currentIsOneOf(Operators))
        {
            auto n = allocate!ExpressionType;
            n.line = current.line;
            n.column = current.column;
            static if (__traits(hasMember, ExpressionType, "operator"))
                n.operator = advance().type;
            else
                advance();
            n.left = node;
            mixin ("n.right = parse" ~ ExpressionPartType.stringof ~ "();");
            node = n;
        }
        return node;
    }

    ListType parseCommaSeparatedRule(alias ListType, alias ItemType, bool setLineAndColumn = false)
        (bool allowTrailingComma = false)
    {
        auto node = allocate!ListType;
        static if (setLineAndColumn)
        {
            node.line = current.line;
            node.column = current.column;
        }
        static if (is(ItemType : ExpressionNode))
            ExpressionNode[] items;
        else
            ItemType[] items;
        while (moreTokens())
        {
            mixin ("auto i = parse" ~ ItemType.stringof ~ "();");
            if (i !is null)
                items ~= i;
            else
            {
                deallocate(node);
                return null;
            }
            if (currentIs(tok!","))
            {
                advance();
                if (allowTrailingComma && currentIsOneOf(tok!")",
                    tok!"}", tok!"]"))
                {
                    break;
                }
                else
                    continue;
            }
            else
                break;
        }
        node.items = ownArray(items);
        return node;
    }

    void warn(lazy string message)
    {
        if (suppressMessages > 0)
            return;
        ++warningCount;
        auto column = index < tokens.length ? tokens[index].column : 0;
        auto line = index < tokens.length ? tokens[index].line : 0;
        if (messageFunction is null)
            stderr.writefln("%s(%d:%d)[warn]: %s", fileName, line, column, message);
        else
            messageFunction(fileName, line, column, message, false);
    }

    void error(string message, bool shouldAdvance = true)
    {
        import std.stdio : stderr;
        if (suppressMessages == 0)
        {
            ++errorCount;
            auto column = index < tokens.length ? tokens[index].column : tokens[$ - 1].column;
            auto line = index < tokens.length ? tokens[index].line : tokens[$ - 1].line;
            if (messageFunction is null)
            {
                stderr.writefln("%s(%d:%d)[error]: %s", fileName, line, column, message);
            }
            else
                messageFunction(fileName, line, column, message, true);
        }
        else
            ++suppressedErrorCount;
        while (shouldAdvance && moreTokens())
        {
            if (currentIsOneOf(tok!";", tok!"}",
                tok!")", tok!"]"))
            {
                advance();
                break;
            }
            else
                advance();
        }
    }

    void skip(alias O, alias C)()
    {
        assert (currentIs(O), current().text);
        advance();
        int depth = 1;
        while (moreTokens())
        {
            switch (tokens[index].type)
            {
                case C:
                    advance();
                    depth--;
                    if (depth <= 0)
                        return;
                    break;
                case O:
                    depth++;
                    advance();
                    break;
                default:
                    advance();
                    break;
            }
        }
    }

    void skipBraces()
    {
        skip!(tok!"{", tok!"}")();
    }

    void skipParens()
    {
        mixin(traceEnterAndExit!(__FUNCTION__));
        skip!(tok!"(", tok!")")();
    }

    void skipBrackets()
    {
        skip!(tok!"[", tok!"]")();
    }

    const(Token)* peek() const
    {
        return index + 1 < tokens.length ? &tokens[index + 1] : null;
    }

    const(Token)* peekPast(alias O, alias C)() const nothrow
    {
        if (index >= tokens.length)
            return null;
        int depth = 1;
        size_t i = index;
        ++i;
        while (i < tokens.length)
        {
            if (tokens[i] == O)
            {
                ++depth;
                ++i;
            }
            else if (tokens[i] == C)
            {
                --depth;
                ++i;
                if (depth <= 0)
                    break;
            }
            else
                ++i;
        }
        return i >= tokens.length ? null : depth == 0 ? &tokens[i] : null;
    }

    const(Token)* peekPastParens() const nothrow
    {
        return peekPast!(tok!"(", tok!")")();
    }

    const(Token)* peekPastBrackets() const nothrow
    {
        return peekPast!(tok!"[", tok!"]")();
    }

    const(Token)* peekPastBraces() const nothrow
    {
        return peekPast!(tok!"{", tok!"}")();
    }

    bool peekIs(IdType t) const nothrow
    {
        return index + 1 < tokens.length && tokens[index + 1].type == t;
    }

    bool peekIsOneOf(IdType[] types...) const nothrow
    {
        if (index + 1 >= tokens.length) return false;
        return canFind(types, tokens[index + 1].type);
    }

    /**
     * Returns a token of the specified type if it was the next token, otherwise
     * calls the error function and returns null.
     */
    const(Token)* expect(IdType type)
    {
        if (index < tokens.length && tokens[index].type == type)
            return &tokens[index++];
        else
        {
            string tokenString = str(type) is null
                ? to!string(type) : str(type);
            bool shouldNotAdvance = index < tokens.length
                && (tokens[index].type == tok!")"
                || tokens[index].type == tok!";"
                || tokens[index].type == tok!"}");
            auto token = (index < tokens.length
                ? (tokens[index].text is null ? str(tokens[index].type) : tokens[index].text)
                : "EOF");
            error("Expected " ~  tokenString ~ " instead of " ~ token,
                !shouldNotAdvance);
            return null;
        }
    }

    /**
     * Returns: the _current token
     */
    Token current() const @property
    {
        return tokens[index];
    }

    /**
     * Advances to the next token and returns the current token
     */
    Token advance()
    {
        return tokens[index++];
    }

    /**
     * Returns: true if the current token has the given type
     */
    bool currentIs(IdType type) const
    {
        return index < tokens.length && tokens[index] == type;
    }

    /**
     * Returns: true if the current token is one of the given types
     */
    bool currentIsOneOf(IdType[] types...) const
    {
        if (index >= tokens.length)
            return false;
        return canFind(types, current.type);
    }

    bool startsWith(IdType[] types...) const nothrow
    {
        if (index + types.length >= tokens.length)
            return false;
        for (size_t i = 0; (i < types.length) && ((index + i) < tokens.length); ++i)
        {
            if (tokens[index + i].type != types[i])
                return false;
        }
        return true;
    }

    alias Bookmark = size_t;

    Bookmark setBookmark()
    {
//        mixin(traceEnterAndExit!(__FUNCTION__));
        ++suppressMessages;
        return index;
    }

    void abandonBookmark(Bookmark) nothrow
    {
        --suppressMessages;
        if (suppressMessages == 0)
            suppressedErrorCount = 0;
    }

    void goToBookmark(Bookmark bookmark) nothrow
    {
        --suppressMessages;
        if (suppressMessages == 0)
            suppressedErrorCount = 0;
        index = bookmark;
    }

    template simpleParse(NodeType, parts ...)
    {
        static if (__traits(hasMember, NodeType, "comment"))
            enum simpleParse = "auto node = allocate!" ~ NodeType.stringof ~ ";\n"
                ~ "node.comment = comment;\n"
                ~ "comment = null;\n"
                ~ simpleParseItems!(parts)
                ~ "\nreturn node;\n";
        else
            enum simpleParse = "auto node = allocate!" ~ NodeType.stringof ~ ";\n"
                ~ simpleParseItems!(parts)
                ~ "\nreturn node;\n";
    }

    template simpleParseItems(items ...)
    {
        static if (items.length > 1)
            enum simpleParseItems = simpleParseItem!(items[0]) ~ "\n"
                ~ simpleParseItems!(items[1 .. $]);
        else static if (items.length == 1)
            enum simpleParseItems = simpleParseItem!(items[0]);
        else
            enum simpleParseItems = "";
    }

    template simpleParseItem(alias item)
    {
        static if (is (typeof(item) == string))
            enum simpleParseItem = "if ((node." ~ item[0 .. item.countUntil("|")]
                ~ " = " ~ item[item.countUntil("|") + 1 .. $] ~ "()) is null) { deallocate(node); return null; }";
        else
            enum simpleParseItem = "if (expect(" ~ item.stringof ~ ") is null) { deallocate(node); return null; }";
    }

    template expectSequence(sequence ...)
    {
        static if (sequence.length == 1)
            enum expectSequence = "if (expect(" ~ sequence[0].stringof ~ ") is null) { deallocate(node); return null; }";
        else
            enum expectSequence = "if (expect(" ~ sequence[0].stringof ~ ") is null) { deallocate(node); return null; }\n"
                ~ expectSequence!(sequence[1..$]);
    }

    template traceEnterAndExit(string fun)
    {
        enum traceEnterAndExit = `version (std_parser_verbose) { _traceDepth++; trace("`
            ~ `\033[01;32m` ~ fun ~ `\033[0m"); }`
            ~ `version (std_parser_verbose) scope(exit) { trace("`
            ~ `\033[01;31m` ~ fun ~ `\033[0m"); _traceDepth--; }`;
    }

    version (std_parser_verbose)
    {
        void trace(string message)
        {
            if (suppressMessages > 0)
                return;
            auto depth = format("%4d ", _traceDepth);
            if (index < tokens.length)
                writeln(depth, message, "(", current.line, ":", current.column, ")");
            else
                writeln(depth, message, "(EOF:0)");
        }
    }
    else
    {
        void trace(lazy string) {}
    }

    template nullCheck(string exp)
    {
        enum nullCheck = "{if ((" ~ exp ~ ") is null) { deallocate(node); return null; }}";
    }

    template tokenCheck(string exp, string tok)
    {
        enum tokenCheck = `{auto t = expect(tok!"` ~ tok ~ `");`
            ~ `if (t is null) { deallocate(node); return null;}`
            ~ `else {` ~ exp ~ ` = *t; }}`;
    }

    // This list MUST BE MAINTAINED IN SORTED ORDER.
    static immutable string[] REGISTER_NAMES = [
        "AH", "AL", "AX", "BH", "BL", "BP", "BPL", "BX", "CH", "CL", "CR0", "CR2",
        "CR3", "CR4", "CS", "CX", "DH", "DI", "DIL", "DL", "DR0", "DR1", "DR2",
        "DR3", "DR6", "DR7", "DS", "DX", "EAX", "EBP", "EBX", "ECX", "EDI", "EDX",
        "ES", "ESI", "ESP", "FS", "GS", "MM0", "MM1", "MM2", "MM3", "MM4", "MM5",
        "MM6", "MM7", "R10", "R10B", "R10D", "R10W", "R11", "R11B", "R11D", "R11W",
        "R12", "R12B", "R12D", "R12W", "R13", "R13B", "R13D", "R13W", "R14", "R14B",
        "R14D", "R14W", "R15", "R15B", "R15D", "R15W", "R8", "R8B", "R8D", "R8W",
        "R9", "R9B", "R9D", "R9W", "RAX", "RBP", "RBX", "RCX", "RDI", "RDX", "RSI",
        "RSP", "SI", "SIL", "SP", "SPL", "SS", "ST", "TR3", "TR4", "TR5", "TR6",
        "TR7", "XMM0", "XMM1", "XMM10", "XMM11", "XMM12", "XMM13", "XMM14", "XMM15",
        "XMM2", "XMM3", "XMM4", "XMM5", "XMM6", "XMM7", "XMM8", "XMM9", "YMM0",
        "YMM1", "YMM10", "YMM11", "YMM12", "YMM13", "YMM14", "YMM15", "YMM2",
        "YMM3", "YMM4", "YMM5", "YMM6", "YMM7", "YMM8", "YMM9"
    ];

    enum string BUILTIN_TYPE_CASES = q{
        case tok!"int":
        case tok!"uint":
        case tok!"double":
        case tok!"idouble":
        case tok!"float":
        case tok!"ifloat":
        case tok!"short":
        case tok!"ushort":
        case tok!"long":
        case tok!"ulong":
        case tok!"char":
        case tok!"wchar":
        case tok!"dchar":
        case tok!"bool":
        case tok!"void":
        case tok!"cent":
        case tok!"ucent":
        case tok!"real":
        case tok!"ireal":
        case tok!"byte":
        case tok!"ubyte":
        case tok!"cdouble":
        case tok!"cfloat":
        case tok!"creal":
    };

    enum string LITERAL_CASES = q{
        case tok!"doubleLiteral":
        case tok!"floatLiteral":
        case tok!"idoubleLiteral":
        case tok!"ifloatLiteral":
        case tok!"intLiteral":
        case tok!"longLiteral":
        case tok!"realLiteral":
        case tok!"irealLiteral":
        case tok!"uintLiteral":
        case tok!"ulongLiteral":
        case tok!"stringLiteral":
        case tok!"wstringLiteral":
        case tok!"dstringLiteral":
        case tok!"characterLiteral":
    };

    enum string SPECIAL_CASES = q{
        case tok!"__DATE__":
        case tok!"__EOF__":
        case tok!"__FILE__":
        case tok!"__FUNCTION__":
        case tok!"__LINE__":
        case tok!"__MODULE__":
        case tok!"__PRETTY_FUNCTION__":
        case tok!"__TIME__":
        case tok!"__TIMESTAMP__":
        case tok!"__VENDOR__":
        case tok!"__VERSION__":
    };

    enum string PARSE_INTERFACE_OR_CLASS = q{
        auto ident = expect(tok!"identifier");
        mixin (nullCheck!`ident`);
        node.name = *ident;
        node.comment = comment;
        comment = null;
        if (currentIs(tok!";"))
            goto emptyBody;
        if (currentIs(tok!"{"))
            goto structBody;
        templateStuff: if (currentIs(tok!"("))
        {
            mixin (nullCheck!`node.templateParameters = parseTemplateParameters()`);
            if (currentIs(tok!";"))
                goto emptyBody;
            constraint: if (currentIs(tok!"if"))
                mixin (nullCheck!`node.constraint = parseConstraint()`);
            if (node.baseClassList !is null)
            {
                if (currentIs(tok!"{"))
                    goto structBody;
                else if (currentIs(tok!";"))
                    goto emptyBody;
                else
                {
                    error("Struct body or ';' expected");
                    return null;
                }
            }
            if (currentIs(tok!":"))
                goto baseClassList;
            if (currentIs(tok!"if"))
                goto constraint;
            if (currentIs(tok!";"))
                goto emptyBody;
        }
        if (currentIs(tok!":"))
        {
    baseClassList:
            advance(); // :
            if ((node.baseClassList = parseBaseClassList()) is null)
                return null;
            if (currentIs(tok!"if"))
                goto constraint;
        }
    structBody:
        mixin (nullCheck!`node.structBody = parseStructBody()`);
        return node;
    emptyBody:
        advance();
        return node;
    };

    const(Token)[] tokens;
    int suppressMessages;
    size_t index;
    int _traceDepth;
    string comment;
}

