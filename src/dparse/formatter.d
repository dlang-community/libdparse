#!dmd -g
module dparse.formatter;

import std.algorithm;
import std.range;
import std.stdio;
import std.typetuple:TypeTuple;

import dparse.ast;
import dparse.lexer;
version (unittest)
{
    import dparse.parser;
    import dparse.rollback_allocator;
    import std.array : Appender;
    import std.algorithm : canFind;
}

//debug = verbose;

/**
 * The only brace styles worth using.
 */
enum IndentStyle
{
    /**
     * ---
     * if (something)
     * {
     *     foo();
     *     bar();
     * }
     * else
     * {
     *     bar();
     *     baz();
     * }
     * ---
     */
    allman,
    /**
     * ---
     * if (something) {
     *     foo();
     *     bar();
     * } else {
     *     bar();
     *     baz();
     * }
     * ---
     */
    otbs,
}

/**
 *
 */
void format(Sink, T)(Sink sink, T node, bool useTabs = false,
    IndentStyle style = IndentStyle.allman, uint indentWith = 4)
{
    Formatter!Sink formatter = new Formatter!(Sink)(sink, useTabs, style, indentWith);
    formatter.format(node);
}

///
class Formatter(Sink)
{
    /**
     * Params:
     *     sink = the output range that the formatted source code is placed in
     *     useTabs = if true, tabs are used for indent levels instead of spaces
     *     style = the brace style
     *     indentWidth = the number of spaces used for indentation if useTabs is false
     */
    this(Sink sink, bool useTabs = false, IndentStyle style = IndentStyle.allman, uint indentWidth = 4)
    {
        this.sink = sink;
        this.useTabs = useTabs;
        this.style = style;
        this.indentWidth = indentWidth;
    }

    ///
    void format(const AddExpression addExpression)
    {
        debug(verbose) writeln("AddExpression");
        mixin(binary("addExpression"));
    }

    ///
    void format(const AliasDeclaration aliasDeclaration, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("AliasDeclaration");

        /**
        LinkageAttribute linkageAttribute;
        Type type;
        Token name;
        AliasInitializer[] initializers;
        string comment;
        Parameters parameters;
        MemberFunctionAttribute[] memberFunctionAttributes;
        **/

        with(aliasDeclaration)
        {
            newThing(What.other);
            putComment(comment);
            putAttrs(attrs);
            put("alias ");

            if (initializers.length) // alias ident = a, ident2 = b, etc
            {
                foreach(count, init; initializers)
                {
                    if (count)
                        put(", ");
                    format(init);
                }
            }
            else
            {
                foreach (storageClass; storageClasses)
                {
                    format(storageClass);
                    space();
                }

                if (type)
                {
                    format(type);
                    space();
                }
                if (declaratorIdentifierList)
                {
                    format(declaratorIdentifierList);
                }
                if (parameters)
                {
                    format(parameters);
                }
                foreach(a; memberFunctionAttributes)
                {
                    space();
                    format(a);
                }
            }
            put(";");
        }
    }

    void format(const AliasInitializer aliasInitializer)
    {
        debug(verbose) writeln("AliasInitializer");

        /**
        Token name;
        StorageClass[] storageClasses;
        TemplateParameters templateParameters;
        Type type;
        FunctionLiteralExpression functionLiteralExpression;
        Parameters parameters;
        MemberFunctionAttribute[] memberFunctionAttributes;
        */

        with(aliasInitializer)
        {
            format(name);
            if (templateParameters)
                format(templateParameters);
            put(" = ");
            if (type)
                format(type);
            if (functionLiteralExpression)
                format(functionLiteralExpression);
            if (parameters)
                format(parameters);
            foreach(att; memberFunctionAttributes)
            {
                space();
                format(att);
            }
        }
    }

    void format(const AliasThisDeclaration decl, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("AliasThisDeclaration");

        /**
        Token identifier;
        **/

        putAttrs(attrs);
        put("alias ");
        format(decl.identifier);
        put(" this;");
    }

    void format(const AlignAttribute alignAttribute)
    {
        debug(verbose) writeln("AlignAttribute");

        /**
        Token intLiteral;
        **/

        put("align");
        if (alignAttribute.assignExpression !is null)
        {
            put("(");
            format(alignAttribute.assignExpression);
            put(")");
        }
    }

    void format(const AndAndExpression andAndExpression)
    {
        debug(verbose) writeln("AndAndExpression");

        with(andAndExpression)
        {
            format(left);
            if (right)
            {
                put(" && ");
                format(right);
            }
        }
    }

    void format(const AndExpression andExpression)
    {
        debug(verbose) writeln("AndExpression");

        with(andExpression)
        {
            format(left);
            if (right)
            {
                put(" & ");
                format(right);
            }
        }
    }

    void format(const ArgumentList argumentList)
    {
        debug(verbose) writeln("ArgumentList");

        foreach(count, arg; argumentList.items)
        {
            if (count) put(", ");
            format(arg);
        }
    }

    void format(const Arguments arguments)
    {
        debug(verbose) writeln("Arguments");

        put("(");
        if (arguments.argumentList) format(arguments.argumentList);
        put(")");
    }

    void format(const ArrayInitializer arrayInitializer)
    {
        debug(verbose) writeln("ArrayInitializer");

        /**
        ArrayMemberInitialization[] arrayMemberInitializations;
        **/

        put("[");
        foreach(count, init; arrayInitializer.arrayMemberInitializations)
        {
            format(init);
            if (count < arrayInitializer.arrayMemberInitializations.length - 1)
                put(", ");
        }
        put("]");
    }

    void format(const ArrayLiteral arrayLiteral)
    {
        debug(verbose) writeln("ArrayLiteral");

        /**
        ArgumentList argumentList;
        **/
        put("[");
        if (arrayLiteral.argumentList)
            format(arrayLiteral.argumentList);
        put("]");
    }

    void format(const ArrayMemberInitialization arrayMemberInitialization)
    {
        debug(verbose) writeln("ArrayMemberInitialization");

        /**
        AssignExpression assignExpression;
        NonVoidInitializer nonVoidInitializer;
        **/

        with(arrayMemberInitialization)
        {
            if (assignExpression)
            {
                format(assignExpression);
                put(":");
            }
            if (nonVoidInitializer)
                format(nonVoidInitializer);

        }
    }

    void format(const AsmAddExp asmAddExp)
    {
        assert(false);
    }

    void format(const AsmAndExp asmAndExp)
    {
        assert(false);
    }

    void format(const AsmBrExp asmBrExp)
    {
        assert(false);
    }

    void format(const AsmEqualExp asmEqualExp)
    {
        assert(false);
    }

    void format(const AsmExp asmExp)
    {
        assert(false);
    }

    void format(const AsmInstruction asmInstruction)
    {
        assert(false);
    }

    void format(const AsmLogAndExp asmLogAndExp)
    {
        assert(false);
    }

    void format(const AsmLogOrExp asmLogOrExp)
    {
        assert(false);
    }

    void format(const AsmMulExp asmMulExp)
    {
        assert(false);
    }

    void format(const AsmOrExp asmOrExp)
    {
        assert(false);
    }

    void format(const AsmPrimaryExp asmPrimaryExp)
    {
        assert(false);
    }

    void format(const AsmRelExp asmRelExp)
    {
        assert(false);
    }

    void format(const AsmShiftExp asmShiftExp)
    {
        assert(false);
    }

    void format(const AsmStatement asmStatement)
    {
        assert(false);
    }

    void format(const AsmTypePrefix asmTypePrefix)
    {
        assert(false);
    }

    void format(const AsmUnaExp asmUnaExp)
    {
        assert(false);
    }

    void format(const AsmXorExp asmXorExp)
    {
        assert(false);
    }

    void format(const AssertArguments assertArguments)
    {
        debug(verbose) writeln("AssertExpression");

        /**
        AssignExpression assertion;
        AssignExpression message;
        **/

        with(assertArguments)
        {
            format(assertion);
            if (message)
            {
                put(", ");
                format(message);
            }
        }
    }

    void format(const AssertExpression assertExpression)
    {
        debug(verbose) writeln("AssertExpression");

        /**
        AssertArguments assertArguments;
        **/

        with(assertExpression)
        {
            put("assert (");
            format(assertArguments);
            put(")");
        }
    }

    void format(const AssignExpression assignExpression)
    {
        debug(verbose) writeln("AssignExpression");

        /**
        ExpressionNode ternaryExpression;
        ExpressionNode assignExpression;
        IdType operator;
        **/

        if (assignExpression.ternaryExpression)
            format(assignExpression.ternaryExpression);

        if(assignExpression.expression)
        {
            space();
            put(tokenRep(assignExpression.operator));
            space();
            format(assignExpression.expression);
        }
    }

    void format(const AssocArrayLiteral assocArrayLiteral)
    {
        debug(verbose) writeln("AssocArrayLiteral");

        /**
        KeyValuePairs keyValuePairs;
        **/

        put("[");
        format(assocArrayLiteral.keyValuePairs);
        put("]");
    }

    void format(const AtAttribute atAttribute)
    {
        debug(verbose) writeln("AtAttribute");

        /**
        FunctionCallExpression functionCallExpression;
        ArgumentList argumentList;
        Token identifier;
        **/

        with(atAttribute)
        {
            put("@");
            format(identifier);
            if(argumentList) format(argumentList);
        }
    }

    void format(const Attribute att)
    {
        debug(verbose) writeln("Attribute");

        /**
        LinkageAttribute linkageAttribute;
        AlignAttribute alignAttribute;
        PragmaExpression pragmaExpression;
        StorageClass storageClass;
        IdType attribute;
        **/

        with(att)
        {
            if (pragmaExpression) format(pragmaExpression);
            if (attribute.type != tok!"") put(tokenRep(attribute.type));
            if (identifierChain)
            {
                put("(");
                format(identifierChain);
                put(")");
            }
            if (deprecated_) format(deprecated_);
            if (atAttribute) format(atAttribute);
            if (linkageAttribute) format(linkageAttribute);
        }
    }

    void format(const AttributeDeclaration decl, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("AttributeDeclaration");

        auto cIndent = indentLevel;
        outdent();
        newThing(What.attributeDecl);
        putAttrs(attrs);
        format(decl.attribute);
        put(":");
        indentLevel = cIndent;
    }

    void format(const AutoDeclaration decl)
    {
        debug(verbose) writeln("AutoDeclaration");

        /**
        StorageClasses[] storageClasses;
        Token[] identifiers;
        Initializer[] initializers;
        **/

        foreach(sc; decl.storageClasses)
        {
            format(sc);
            space();
        }

        // zip doesn't work here, dmd 2.064.2
        foreach(i, part; decl.parts)
        {
            if (i > 0) put(", ");
            format(part.identifier);
            if (part.templateParameters)
                format(part.templateParameters);
            put(" = ");
            format(part.initializer);
        }
        put(";");
    }

    void format(const BlockStatement blockStatement)
    {
        debug(verbose) writeln("BlockStatement");

        if (blockStatement.declarationsAndStatements is null)
        {
            space();
            put("{}");
        }
        else
        {
            startBlock();
            format(blockStatement.declarationsAndStatements);
            endBlock();
        }
    }
    void format(const BreakStatement breakStatement)
    {
        debug(verbose) writeln("BreakStatement");

        put("break");
        if (breakStatement.label != tok!"")
        {
            space();
            format(breakStatement.label);
        }
        put(";");
    }

    void format(const BaseClass baseClass)
    {
        debug(verbose) writeln("BaseClass");
        with(baseClass)
        {
            if (type2) format(type2);
        }
    }

    void format(const BaseClassList baseClassList)
    {
        debug(verbose) writeln("BaseClassList");
        put(" : ");
        foreach(count, item; baseClassList.items)
        {
            format(item);
            if (count < baseClassList.items.length - 1)
                put(", ");
        }
    }

    void format(const CaseRangeStatement caseRangeStatement)
    {
        debug(verbose) writeln("CaseRangeStatement");

        /**
        AssignExpression low;
        AssignExpression high;
        DeclarationsAndStatements declarationsAndStatements;
        **/

        with(caseRangeStatement)
        {
            if (low)
            {
                put("case ");
                format(low);
                put(": .. ");
            }
            put("case ");
            format(high);
            put(":");

            formatCaseDecls(declarationsAndStatements);
        }
    }

    void format(const CaseStatement caseStatement)
    {
        debug(verbose) writeln("CaseStatement");

        /**
        ArgumentList argumentList;
        DeclarationsAndStatements declarationsAndStatements;
        **/

        with(caseStatement)
        {
            if (argumentList)
            {
                put("case ");
                format(argumentList);
                put(":");
            }

            formatCaseDecls(declarationsAndStatements);
        }
    }

    void format(const CastExpression castExpression)
    {
        debug(verbose) writeln("CastExpression");

        /**
        Type type;
        CastQualifier castQualifier;
        UnaryExpression unaryExpression;
        **/

        with(castExpression)
        {
            put("cast(");
            if (castQualifier)
            {
                format(castQualifier);
                space();
            }
            if (type) format(type);
            put(")");
            if (unaryExpression) format(unaryExpression);
        }
    }

    void format(const CastQualifier qual)
    {
        debug(verbose) writeln("CastQualifier");

        /**
        Token first;
        Token second;
        **/

        format(qual.first);
        if (qual.second != tok!"")
        {
            space();
            format(qual.second);
        }
    }

    void format(const Catch catch_)
    {
        debug(verbose) writeln("Catch");

        /**
        Type type;
        Token identifier;
        DeclarationOrStatement declarationOrStatement;
        **/

        with(catch_)
        {
            newThing(What.catch_);
            put("catch(");
            format(type);
            if (identifier != tok!"")
            {
                space();
                format(identifier);
            }
            put(")");
            if (declarationOrStatement) maybeIndent(declarationOrStatement);
        }
    }

    void format(const Catches catches)
    {
        debug(verbose) writeln("Catches");

        /**
        Catch[] catches;
        LastCatch lastCatch;
        **/

        foreach(c; catches.catches)
            format(c);
        if (catches.lastCatch)
            format(catches.lastCatch);
    }

    void format(const ClassDeclaration decl, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("ClassDeclaration");

        /**
        Token name;
        TemplateParameters templateParameters;
        Constraint constraint;
        BaseClassList baseClassList;
        StructBody structBody;
        string comment;
        **/

        newThing(What.aggregateDecl);
        putComment(decl.comment);
        putAttrs(attrs);

        put("class ");
        format(decl.name);

        if (decl.templateParameters)
            format(decl.templateParameters);

        if (decl.constraint)
        {
            space();
            format(decl.constraint);
        }

        if (decl.baseClassList)
        {
            format(decl.baseClassList);
        }

        format(decl.structBody);
    }

    void format(const CmpExpression cmpExpression)
    {
        debug(verbose) writeln("CmpExpression");

        /**
        ExpressionNode shiftExpression;
        ExpressionNode equalExpression;
        ExpressionNode identityExpression;
        ExpressionNode relExpression;
        ExpressionNode inExpression;
        **/

        with(cmpExpression)
        {
            if (shiftExpression) format(shiftExpression);
            else if (equalExpression) format(equalExpression);
            else if (identityExpression) format(identityExpression);
            else if (relExpression) format(relExpression);
            else if (inExpression) format(inExpression);
        }
    }

    void format(const CompileCondition compileCondition)
    {
        debug(verbose) writeln("CompileCondition");

        /**
        VersionCondition versionCondition;
        DebugCondition debugCondition;
        StaticIfCondition staticIfCondition;
        **/

        with(compileCondition)
        {
            if (versionCondition) format(versionCondition);
            else if (debugCondition) format(debugCondition);
            else if (staticIfCondition) format(staticIfCondition);
        }
    }

    void format(const ConditionalDeclaration decl, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("ConditionalDeclaration");

        /**
        CompileCondition compileCondition;
        Declaration[] trueDeclarations;
        Declaration[] falseDeclaration;
        **/

        newThing(What.conditionalDecl);
        putAttrs(attrs);
        format(decl.compileCondition);

        if (decl.trueDeclarations.length > 1)
        {
            startBlock();
            foreach (d; decl.trueDeclarations)
                format(d);
            endBlock();
        }
        else if (decl.trueDeclarations.length == 1)
            maybeIndent(decl.trueDeclarations[0]);
        else
            put("{}");

        if (decl.falseDeclarations.length > 0)
            put("else");

        if (decl.falseDeclarations.length > 1)
        {
            startBlock();
            foreach (d; decl.falseDeclarations)
                format(d);
            endBlock();
        }
        else if (decl.falseDeclarations.length == 1)
            maybeIndent(decl.falseDeclarations[0]);
    }

    void format(const ConditionalStatement stmnt)
    {
        debug(verbose) writeln("ConditionalStatement");

        /**
        CompileCondition compileCondition;
        DeclarationOrStatement trueStatement;
        DeclarationOrStatement falseStatement;
        **/

        newThing(What.conditionalDecl);
        if (stmnt.compileCondition)
            format(stmnt.compileCondition);

        if (stmnt.trueStatement)
            maybeIndent(stmnt.trueStatement);

        if (stmnt.falseStatement)
        {
            newThing(What.else_);
            put("else ");

            // else if...
            if (stmnt.falseStatement.statement &&
                stmnt.falseStatement.statement.statementNoCaseNoDefault &&
                stmnt.falseStatement.statement.statementNoCaseNoDefault.conditionalStatement)
            {
                format(stmnt.falseStatement.statement.statementNoCaseNoDefault.conditionalStatement);
                return;
            }

            maybeIndent(stmnt.falseStatement);
        }
    }

    void format(const Constraint constraint)
    {
        debug(verbose) writeln("Constraint");

        if (constraint.expression)
        {
            indent();
            newline();
            put("if(");
            format(constraint.expression);
            put(")");
            outdent();
        }
    }

    void format(const Constructor constructor, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("Constructor");

        /**
        Parameters parameters;
        FunctionBody functionBody;
        Constraint constraint;
        MemberFunctionAttribute[] memberFunctionAttributes;
        TemplateParameters templateParameters;
        size_t location;
        string comment;
        **/

        newThing(What.functionDecl);
        putComment(constructor.comment);
        putAttrs(attrs);

        put("this");

        if (constructor.templateParameters)
            format(constructor.templateParameters);

        if (constructor.parameters)
            format(constructor.parameters);

        foreach(att; constructor.memberFunctionAttributes)
        {
            space();
            format(att);
        }

        if (constructor.constraint)
        {
            space();
            format(constructor.constraint);
        }

        if (constructor.functionBody)
            format(constructor.functionBody);
        else
            put(";");
    }

    void format(const ContinueStatement continueStatement)
    {
        debug(verbose) writeln("ContinueStatement");

        put("continue");
        if (continueStatement.label != tok!"")
        {
            space();
            format(continueStatement.label);
        }
        put(";");
    }

    void format(const DebugCondition debugCondition)
    {
        debug(verbose) writeln("DebugCondition");

        put("debug");
        if (debugCondition.identifierOrInteger != tok!"")
        {
            put("(");
            format(debugCondition.identifierOrInteger);
            put(")");
        }
    }

    void format(const DebugSpecification debugSpecification)
    {
        debug(verbose) writeln("DebugSpecification");

        newThing(What.other);
        put("debug = ");
        format(debugSpecification.identifierOrInteger);
        put(";");
    }

    void format(const Declaration declaration)
    {
        debug(verbose) writeln("Declaration");

        with(declaration)
        {
            string mix(string[] s) {
                string r;
                foreach(c, d; s)
                    r ~= (c > 0 ? "else " : "") ~ "if (" ~ d ~ ") { format(" ~ d ~ ", attributes); }";
                return r;
            }

            mixin(mix(possibleDeclarations));

            if (declarations.length)
            {
                putAttrs(attributes);
                startBlock();
                foreach(d; declarations)
                    format(d);
                endBlock();
            }
        }
    }

    void format(const DeclarationOrStatement declarationsOrStatement)
    {
        debug(verbose) writeln("DeclarationOrStatement");

        with(declarationsOrStatement)
            declaration !is null ? format(declaration) : format(statement);
    }

    void format(const DeclarationsAndStatements declarationsAndStatements)
    {
        debug(verbose) writeln("DeclarationsAndStatements");

        foreach(ds; declarationsAndStatements.declarationsAndStatements)
            format(ds);
    }

    void format(const Declarator declarator)
    {
        debug(verbose) writeln("Declarator");

        /**
        Token name;
        Initializer initializer;
        **/

        format(declarator.name);

        foreach(suffix; declarator.cstyle)
            format(suffix);

        if (declarator.templateParameters)
            format(declarator.templateParameters);

        if (declarator.initializer)
        {
            put(" = ");
            format(declarator.initializer);
        }
    }

    void format(const DefaultStatement defaultStatement)
    {
        debug(verbose) writeln("DefaultStatement");

        /**
        DeclarationsAndStatements declarationsAndStatements;
        **/

        put("default:");
        formatCaseDecls(defaultStatement.declarationsAndStatements);
    }

    void format(const DeleteExpression deleteExpression)
    {
        debug(verbose) writeln("DeleteExpression");

        put("delete ");
        format(deleteExpression.unaryExpression);
    }

    void format(const DeleteStatement deleteStatement)
    {
        debug(verbose) writeln("DeleteStatement");

        format(deleteStatement.deleteExpression);
        put(";");
    }

    void format(const Deprecated deprecated_)
    {
        debug (verbose)
            writeln("Deprecated");
        put("deprecated");
        if (deprecated_.assignExpression !is null)
        {
            put("(");
            format(deprecated_.assignExpression);
            put(")");
        }
    }

    void format(const Destructor destructor, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("Destructor");

        /**
        FunctionBody functionBody;
        **/

        newThing(What.functionDecl);
        putAttrs(attrs);
        put("~this()");

        if (destructor.functionBody)
            format(destructor.functionBody);
        else
            put(";");
    }

    void format(const DoStatement doStatement)
    {
        debug(verbose) writeln("DoStatement");

        /**
        StatementNoCaseNoDefault statementNoCaseNoDefault;
        Expression expression;
        **/

        with(doStatement)
        {
            newThing(What.other);
            put("do");
            if (statementNoCaseNoDefault) format(statementNoCaseNoDefault);
            space();
            put("while (");
            format(expression);
            put(");");
        }
    }

    void format(const EnumBody enumBody)
    {
        debug(verbose) writeln("EnumBody");

        newline();
        startBlock();
        foreach(count, member; enumBody.enumMembers)
        {
            format(member);

            if (count < enumBody.enumMembers.length - 1)
                put(",");

            if (member.comment.length)
            {
                space();
                put(member.comment);
            }
        }
        endBlock();
    }

    void format(const EnumDeclaration enumDeclaration, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("EnumDeclaration");

        /**
        Token name;
        Type type;
        EnumBody enumBody;
        string comment;
        **/

        with(enumDeclaration)
        {
            newThing(What.aggregateDecl);
            putComment(comment);
            putAttrs(attrs);
            put("enum ");
            if (name != tok!"")
            {
                format(name);
                space();
            }
            if (type)
            {
                put(": ");
                format(type);
                space();
            }
            if (enumBody) format(enumBody);
        }
    }

    void format(const EnumMember enumMember)
    {
        debug(verbose) writeln("EnumMember");

        /**
        Token name;
        Type type;
        AssignExpression assignExpression;
        string comment;
        **/

        with(enumMember)
        {
            enumMemberAttributes.each!(a => format(a));
            newline();
            if (type) format(type);
            format(name);
            if (assignExpression)
            {
                put(" = ");
                format(assignExpression);
            }
        }
    }

    void format(const EnumMemberAttribute ema)
    {
        if (ema.atAttribute)
            format(ema.atAttribute);
        else if (ema.deprecated_)
            format(ema.deprecated_);
    }

    void format(const EponymousTemplateDeclaration decl)
    {
        debug(verbose) writeln("EponymousTemplateDeclaration");

        /**
        Token name;
        TemplateParameters templateParameters;
        AssignExpression assignExpression;
        **/

        put("enum ");
        put(tokenRep(decl.name));
        if (decl.templateParameters)
            format(decl.templateParameters);
        if (decl.assignExpression)
        {
            put(" = ");
            format(decl.assignExpression);
        }
        put(";");
    }

    void format(const EqualExpression equalExpression)
    {
        debug(verbose) writeln("EqualExpression");

        mixin(binary("equalExpression"));
    }

    void format(const Expression expression)
    {
        debug(verbose) writeln("Expression");

        foreach(count, item; expression.items)
        {
            if (count)
                put(", ");
            format(item);
        }
    }

    void format(const ExpressionNode n)
    {
        debug(verbose) writeln("ExpressionNode");

        if (cast(AddExpression) n) format(cast(AddExpression) n);
        else if (cast(AndAndExpression) n) format(cast(AndAndExpression) n);
        else if (cast(AndExpression) n) format(cast(AndExpression) n);
        else if (cast(AsmAddExp) n) format(cast(AsmAddExp) n);
        else if (cast(AsmAndExp) n) format(cast(AsmAndExp) n);
        else if (cast(AsmEqualExp) n) format(cast(AsmEqualExp) n);
        else if (cast(AsmLogAndExp) n) format(cast(AsmLogAndExp) n);
        else if (cast(AsmLogOrExp) n) format(cast(AsmLogOrExp) n);
        else if (cast(AsmMulExp) n) format(cast(AsmMulExp) n);
        else if (cast(AsmOrExp) n) format(cast(AsmOrExp) n);
        else if (cast(AsmRelExp) n) format(cast(AsmRelExp) n);
        else if (cast(AsmShiftExp) n) format(cast(AsmShiftExp) n);
        else if (cast(AssertExpression) n) format(cast(AssertExpression) n);
        else if (cast(AssignExpression) n) format(cast(AssignExpression) n);
        else if (cast(CmpExpression) n) format(cast(CmpExpression) n);
        else if (cast(DeleteExpression) n) format(cast(DeleteExpression) n);
        else if (cast(EqualExpression) n) format(cast(EqualExpression) n);
        else if (cast(Expression) n) format(cast(Expression) n);
        else if (cast(FunctionCallExpression) n) format(cast(FunctionCallExpression) n);
        else if (cast(FunctionLiteralExpression) n) format(cast(FunctionLiteralExpression) n);
        else if (cast(IdentityExpression) n) format(cast(IdentityExpression) n);
        else if (cast(ImportExpression) n) format(cast(ImportExpression) n);
        else if (cast(IndexExpression) n) format(cast(IndexExpression) n);
        else if (cast(InExpression) n) format(cast(InExpression) n);
        else if (cast(IsExpression) n) format(cast(IsExpression) n);
        else if (cast(MixinExpression) n) format(cast(MixinExpression) n);
        else if (cast(MulExpression) n) format(cast(MulExpression) n);
        else if (cast(NewAnonClassExpression) n) format(cast(NewAnonClassExpression) n);
        else if (cast(NewExpression) n) format(cast(NewExpression) n);
        else if (cast(OrExpression) n) format(cast(OrExpression) n);
        else if (cast(OrOrExpression) n) format(cast(OrOrExpression) n);
        else if (cast(PowExpression) n) format(cast(PowExpression) n);
        else if (cast(PragmaExpression) n) format(cast(PragmaExpression) n);
        else if (cast(PrimaryExpression) n) format(cast(PrimaryExpression) n);
        else if (cast(RelExpression) n) format(cast(RelExpression) n);
        else if (cast(ShiftExpression) n) format(cast(ShiftExpression) n);
        else if (cast(TemplateMixinExpression) n) format(cast(TemplateMixinExpression) n);
        else if (cast(TernaryExpression) n) format(cast(TernaryExpression) n);
        else if (cast(TraitsExpression) n) format(cast(TraitsExpression) n);
        else if (cast(TypeidExpression) n) format(cast(TypeidExpression) n);
        else if (cast(TypeofExpression) n) format(cast(TypeofExpression) n);
        else if (cast(UnaryExpression) n) format(cast(UnaryExpression) n);
        else if (cast(XorExpression) n) format(cast(XorExpression) n);
    }

    void format(const ExpressionStatement expressionStatement)
    {
        debug(verbose) writeln("ExpressionStatement");

        if (expressionStatement.expression)
            format(expressionStatement.expression);
        put(";");
    }

    void format(const FinalSwitchStatement finalSwitchStatement)
    {
        debug(verbose) writeln("FinalSwitchStatement");

        format(finalSwitchStatement.switchStatement, true);
    }

    void format(const Finally finally_)
    {
        debug(verbose) writeln("Finally");

        put("finally");
        format(finally_.declarationOrStatement);
    }

    void format(const ForStatement forStatement)
    {
        debug(verbose) writeln("ForStatement");

        /**
        DeclarationOrStatement initialization;
        ExpressionStatement test;
        Expression increment;
        DeclarationOrStatement declarationOrStatement;
        **/

        with(forStatement)
        {
            newThing(What.other);
            put("for (");
            if (initialization) format(initialization);
            else put(";");
            space();
            if (test) format(test);
            put(";");
            space();
            if (increment) format(increment);
            put(")");

            if (declarationOrStatement) maybeIndent(declarationOrStatement);
        }
    }

    void format(const ForeachStatement foreachStatement)
    {
        debug(verbose) writeln("ForeachStatement");

        /**
        IdType type;
        ForeachTypeList foreachTypeList;
        ForeachType foreachType;
        Expression low;
        Expression high;
        DeclarationOrStatement declarationOrStatement;
        size_t startIndex;
        **/

        with(foreachStatement)
        {
            newThing(What.loop);
            if (type) put(tokenRep(type));
            else assert(false);

            put(" (");
            if (foreachTypeList) format(foreachTypeList);
            else if (foreachType) format(foreachType);

            put("; ");

            if (low) format(low);
            if (high)
            {
                put("..");
                format(high);
            }
            put(")");
            format(declarationOrStatement);
        }
    }

    void format(const ForeachType foreachType)
    {
        debug(verbose) writeln("ForeachType");

        /**
        IdType[] typeConstructors;
        Type type;
        Token identifier;
        **/

        with(foreachType)
        {
            foreach(tp; typeConstructors)
            {
                if (tp) put(tokenRep(tp));
                space();
            }
            if (type)
            {
                format(type);
                space();
            }
            format(identifier);
        }
    }

    void format(const ForeachTypeList foreachTypeList)
    {
        debug(verbose) writeln("ForeachTypeList");

        /**
        ForeachType[] items;
        **/

        foreach(count, item; foreachTypeList.items)
        {
            format(item);
            if (count < foreachTypeList.items.length - 1)
                put(", ");
        }
    }

    void format(const FunctionAttribute functionAttribute)
    {
        debug(verbose) writeln("FunctionAttribute");

        /**
        Token token;
        AtAttribute atAttribute;
        **/

        with(functionAttribute)
        {
            if (token != tok!"")
            {
                format(token);
                space();
            }
            if (atAttribute) format(atAttribute);
        }
    }

    void format(const FunctionBody functionBody)
    {
        debug(verbose) writeln("FunctionBody");

        with(functionBody)
        {
            if (specifiedFunctionBody) format(specifiedFunctionBody);
            if (missingFunctionBody) format(missingFunctionBody);
        }
    }

    void format(const FunctionCallExpression functionCallExpression)
    {
        debug(verbose) writeln("FunctionCallExpression");

        /**
        Type type;
        UnaryExpression unaryExpression;
        TemplateArguments templateArguments;
        Arguments arguments;
        **/

        with(functionCallExpression)
        {
            if (type) format(type);
            if (unaryExpression) format(unaryExpression);
            if (templateArguments) format(templateArguments);
            if (arguments) format(arguments);
        }
    }

    void format(const FunctionContract functionContract)
    {
        debug(verbose) writeln("FunctionContract");

        /**
        InOutContractExpression inOutContractExpression;
        InOutStatement inOutStatement;
        **/

        with(functionContract)
        {
            if (inOutContractExpression) format(inOutContractExpression);
            if (inOutStatement) format(inOutStatement);
        }
    }

    void format(const FunctionDeclaration decl, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("FunctionDeclaration");

        /**
        bool hasAuto;
        bool hasRef;
        **/

        newThing(What.functionDecl);
        putComment(decl.comment);
        putAttrs(attrs);

        foreach (sc; decl.storageClasses)
            format(sc);

        if (decl.returnType)
            format(decl.returnType);

        space();
        format(decl.name);

        if (decl.templateParameters)
            format(decl.templateParameters);
        if (decl.parameters)
            format(decl.parameters);
        foreach(attr; decl.memberFunctionAttributes)
        {
            space();
            format(attr);
        }
        if (decl.constraint)
        {
            space();
            format(decl.constraint);
        }

        if (decl.functionBody)
            format(decl.functionBody);
        else
            put(";");
    }

    void format(const FunctionLiteralExpression functionLiteralExpression)
    {
        debug(verbose) writeln("FunctionLiteralExpression");
        /**
        ExpressionNode assignExpression;
        FunctionAttribute[] functionAttributes;
        SpecifiedFunctionBody specifiedFunctionBody;
        IdType functionOrDelegate;
        MemberFunctionAttribute[] memberFunctionAttributes;
        Parameters parameters;
        Token identifier;
        Type returnType;
        bool isReturnRef;
        **/

        with(functionLiteralExpression)
        {
            put(tokenRep(functionOrDelegate));

            if (isReturnRef) put("ref ");
            //if (returnType || parameters)
              //  space();

            if (returnType) format(returnType);
            if (parameters) format(parameters);

            foreach(att; memberFunctionAttributes)
            {
                space();
                format(att);
            }

            ignoreNewlines = true;
            if (specifiedFunctionBody)
                format(specifiedFunctionBody);
            else
            {
                format(identifier);
                put(" => ");
                format(assignExpression);
            }
            ignoreNewlines = false;
        }
    }

    void format(const GotoStatement gotoStatement)
    {
        debug(verbose) writeln("GotoStatement");

        put("goto ");
        gotoStatement.label != tok!"" ?
            put(tokenRep(gotoStatement.label)) :
                format(gotoStatement.expression);
        put(";");
    }

    void format(const IdentifierChain identifierChain)
    {
        debug(verbose) writeln("IdentifierChain");

        foreach(count, ident; identifierChain.identifiers)
        {
            if (count) put(".");
            put(ident.text);
        }
    }

    void format(const DeclaratorIdentifierList declaratorIdentifierList)
    {
        debug(verbose) writeln("DeclaratorIdentifierList");

        foreach(count, ident; declaratorIdentifierList.identifiers)
        {
            if (count)
                put(", ");
            put(ident.text);
        }
    }

    void format(const TypeIdentifierPart typeIdentifierPart)
    {
        debug(verbose) writeln("TypeIdentifierPart");

        if (typeIdentifierPart.dot)
        {
            put(".");
        }
        if (typeIdentifierPart.identifierOrTemplateInstance)
        {
            format(typeIdentifierPart.identifierOrTemplateInstance);
        }
        if (typeIdentifierPart.indexer)
        {
            put("[");
            format(typeIdentifierPart.indexer);
            put("]");
        }
        if (typeIdentifierPart.typeIdentifierPart)
        {
            put(".");
            format(typeIdentifierPart.typeIdentifierPart);
        }
    }

    void format(const IdentifierOrTemplateChain identifierOrTemplateChain)
    {
        debug(verbose) writeln("IdentifierOrTemplateChain");

        with(identifierOrTemplateChain)
        {
            foreach(count, ident; identifiersOrTemplateInstances)
            {
                if (count) put(".");
                format(ident);
            }
        }
    }

    void format(const IdentifierOrTemplateInstance identifierOrTemplateInstance)
    {
        debug(verbose) writeln("IdentifierOrTemplateInstance");

        with(identifierOrTemplateInstance)
        {
            format(identifier);
            if (templateInstance)
                format(templateInstance);
        }
    }

    void format(const IdentityExpression identityExpression)
    {
        debug(verbose) writeln("IdentityExpression");

        with(identityExpression)
        {
            if (left) format(left);
            put(negated ? " !is " : " is ");
            if (right) format(right);
        }
    }

    void format(const IfStatement ifStatement)
    {
        debug(verbose) writeln("IfStatement");

        /**
        IdType[] typeCtors;
        Type type;
        Token identifier;
        Expression expression;
        DeclarationOrStatement thenStatement;
        DeclarationOrStatement elseStatement;
        **/

        with(ifStatement)
        {
            bool isAuto = identifier != tok!"" && !type;
            bool isAssign = isAuto || type || typeCtors.length;

            put("if (");

            if (isAuto) put("auto ");
            foreach(tct; typeCtors)
            {
                put(str(tct));
                space();
            }
            if (type)
            {
                format(type);
                space();
            }
            if (identifier != tok!"")
            {
                format(identifier);
                space();
            }
            if (isAssign) put("= ");
            if (expression) format(expression);
            put(")");

            if (thenStatement)
                maybeIndent(thenStatement);

            if (elseStatement)
            {
                newThing(What.else_);
                put("else ");

                if (elseStatement.statement &&
                    elseStatement.statement.statementNoCaseNoDefault &&
                    elseStatement.statement.statementNoCaseNoDefault.ifStatement)
                {
                    // this is to stop automatic newlineIndent
                    format(elseStatement.statement.statementNoCaseNoDefault.ifStatement);
                }
                else
                    maybeIndent(elseStatement);
            }

        }
    }

    void format(const ImportBind importBind)
    {
        debug(verbose) writeln("ImportBind");

        format(importBind.left);
        if (importBind.right != tok!"")
        {
            put(" = ");
            format(importBind.right);
        }
    }

    void format(const ImportBindings importBindings)
    {
        debug(verbose) writeln("ImportBindings");

        /**
        SingleImport singleImport;
        ImportBind[] importBinds;
        **/

        with(importBindings)
        {
            format(singleImport);
            put(" : ");
            foreach(count, bind; importBinds)
            {
                if (count) put(", ");
                format(bind);
            }
        }
    }

    void format(const ImportDeclaration importDeclaration, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("ImportDeclaration");

        /**
        SingleImport[] singleImports;
        ImportBindings importBindings;
        **/

        with(importDeclaration)
        {
            newThing(What.importDecl);
            putAttrs(attrs);
            put("import ");

            if (singleImports.length == 1)
            {
                format(singleImports[0]);
            }
            else if (singleImports.length > 1)
            {
                indent();
                newlineIndent();
                foreach(count, imp; singleImports)
                {
                    format(imp);
                    if (count < singleImports.length - 1)
                    {
                        put(", ");
                        newlineIndent();
                    }
                }
                outdent();
            }

            if (importBindings)
            {
                if (singleImports.length)
                    put(", ");
                format(importBindings);
            }
            put(";");
        }
    }

    void format(const ImportExpression importExpression)
    {
        debug(verbose) writeln("ImportExpression");

        put("import (");
        format(importExpression.assignExpression);
        put(");");
    }

    void format(const Index index)
    {
        format(index.low);
        if (index.high !is null)
        {
            put(" .. ");
            format(index.high);
        }
    }

    void format(const IndexExpression indexExpression)
    {
        debug(verbose) writeln("IndexExpression");

        /**
        UnaryExpression unaryExpression;
        ArgumentList argumentList;
        **/

        with(indexExpression)
        {
            format(indexExpression.unaryExpression);
            put("[");
            foreach (i, index; indexes)
            {
                if (i != 0)
                    put(", ");
                format(index);
            }
            put("]");
        }
    }

    void format(const InContractExpression expression)
    {
        debug(verbose) writeln("InContractExpression");
        newlineIndent();
        put("in (");
        format(expression.assertArguments);
        put(")");
    }

    void format(const InExpression inExpression)
    {
        debug(verbose) writeln("InExpression");

        with(inExpression)
        {
            if (left) format(left);
            put(negated ? " !in " : " in ");
            if (right) format(right);
        }
    }

    void format(const InOutContractExpression inOutContractExpression)
    {
        debug(verbose) writeln("InOutContractExpression");

        with(inOutContractExpression)
        {
            if (inContractExpression) format(inContractExpression);
            if (outContractExpression) format(outContractExpression);
        }
    }

    void format(const InOutStatement inOutStatement)
    {
        debug(verbose) writeln("InOutStatement");

        with(inOutStatement)
        {
            if (inStatement) format(inStatement);
            if (outStatement) format(outStatement);
        }
    }

    void format(const InStatement inStatement)
    {
        debug(verbose) writeln("InStatement");
        newlineIndent();
        put("in");
        format(inStatement.blockStatement);
    }

    void format(const Initialize initialize)
    {
        debug(verbose) writeln("Initialize");
        assert(false);
    }

    void format(const Initializer initializer)
    {
        debug(verbose) writeln("Initializer");

        initializer.nonVoidInitializer ? format(initializer.nonVoidInitializer) : put("void");
    }

    void format(const InterfaceDeclaration interfaceDeclaration, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("InterfaceDeclaration");

        /**
        Token name;
        TemplateParameters templateParameters;
        Constraint constraint;
        BaseClassList baseClassList;
        StructBody structBody;
        string comment;
        **/

        with(interfaceDeclaration)
        {
            newThing(What.aggregateDecl);
            putComment(comment);
            putAttrs(attrs);

            put("interface ");
            format(name);
            if (templateParameters) format(templateParameters);
            if (constraint)
            {
                space();
                format(constraint);
            }
            if (baseClassList)
            {
                format(baseClassList);
            }

            if (structBody)
                format(structBody);
            else
                put(";");
        }
    }

    void format(const Invariant invariant_, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("Invariant");

        /**
        BlockStatement blockStatement;
        string comment;
        **/

        putComment(invariant_.comment);
        putAttrs(attrs);
        put("invariant(");
        if (invariant_.blockStatement !is null)
        {
            put(")");
            format(invariant_.blockStatement);
        }
        else
        {
            format(invariant_.assertArguments);
            put(")");
        }
    }

    void format(const IsExpression isExpression)
    {
        debug(verbose) writeln("IsExpression");

        /**
        Type type;
        Token identifier;
        TypeSpecialization typeSpecialization;
        TemplateParameterList templateParameterList;
        IdType equalsOrColon;
        **/

        with(isExpression)
        {
            put("is(");
            if (type) format(type);
            if (identifier != tok!"")
            {
                space();
                format(identifier);
            }

            if (equalsOrColon)
            {
                space();
                put(tokenRep(equalsOrColon));
                space();
            }

            if (typeSpecialization) format(typeSpecialization);
            if (templateParameterList)
            {
                put(", ");
                format(templateParameterList);
            }
            put(")");
        }
    }

    void format(const KeyValuePair keyValuePair)
    {
        debug(verbose) writeln("KeyValuePair");

        /**
        AssignExpression key;
        AssignExpression value;
        **/

        format(keyValuePair.key);
        put(":");
        format(keyValuePair.value);
    }

    void format(const KeyValuePairs keyValuePairs)
    {
        debug(verbose) writeln("KeyValuePairs");

        /**
        KeyValuePair[] keyValuePairs;
        **/

        foreach(count, pair; keyValuePairs.keyValuePairs)
        {
            format(pair);
            if (count + 1 < keyValuePairs.keyValuePairs.length)
                put(", ");
        }
    }

    void format(const LabeledStatement stmt)
    {
        debug(verbose) writeln("LabeledStatement");

        /**
        Token identifier;
        DeclarationOrStatement declarationOrStatement;
        **/

        format(stmt.identifier);
        put(":");
        if (stmt.declarationOrStatement)
            format(stmt.declarationOrStatement);
    }

    void format(const LastCatch lastCatch)
    {
        debug(verbose) writeln("LastCatch");

        put("catch");
        format(lastCatch.statementNoCaseNoDefault);
    }

    void format(const LinkageAttribute linkageAttribute)
    {
        debug(verbose) writeln("LinkageAttribute");

        /**
        Token identifier;
        bool hasPlusPlus;
        IdentifierChain identifierChain;
        **/

        put("extern (");
        format(linkageAttribute.identifier);
        if (linkageAttribute.identifier.text == "Objective")
            put("-C");
        if (linkageAttribute.hasPlusPlus)
        {
            put("++");
            if (linkageAttribute.typeIdentifierPart)
            {
                put(", ");
                format(linkageAttribute.typeIdentifierPart);
            }
            else if (linkageAttribute.classOrStruct == tok!"class")
                put(", class");
            else if (linkageAttribute.classOrStruct == tok!"struct")
                put(", struct");
        }
        put(")");
    }

    void format(const MemberFunctionAttribute memberFunctionAttribute)
    {
        debug(verbose) writeln("MemberFunctionAttribute");

        /**
        IdType tokenType;
        AtAttribute atAttribute;
        **/

        with(memberFunctionAttribute)
        {
            if (tokenType) put(tokenRep(tokenType));
            else format(atAttribute);
        }
    }

    void format(const MissingFunctionBody missingFunctionBody)
    {
        debug(verbose) writeln("MissingFunctionBody");

        with(missingFunctionBody)
        {
            foreach (contract; functionContracts)
                format(contract);
            put(";");
        }
    }

    void format(const MixinDeclaration mixinDeclaration, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("MixinDeclaration");

        /**
        MixinExpression mixinExpression;
        TemplateMixinExpression templateMixinExpression;
        **/

        with(mixinDeclaration)
        {
            putAttrs(attrs);
            if (mixinExpression) format(mixinExpression);
            else format(templateMixinExpression);
            put(";");
        }
    }

    void format(const MixinExpression mixinExpression)
    {
        debug(verbose) writeln("MixinExpression");

        put("mixin (");
        format(mixinExpression.argumentList);
        put(")");
    }

    void format(const MixinTemplateDeclaration mixinTemplateDeclaration, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("MixinTemplateDeclaration");

        putAttrs(attrs);
        put("mixin ");
        format(mixinTemplateDeclaration.templateDeclaration);
    }

    void format(const MixinTemplateName mixinTemplateName)
    {
        debug(verbose) writeln("MixinTemplateName");

        /**
        Symbol symbol;
        IdentifierOrTemplateChain identifierOrTemplateChain;
        TypeofExpression typeofExpression;
        **/

        with(mixinTemplateName)
        {
            if (symbol) format(symbol);
            else
            {
                format(typeofExpression);
                put(".");
                format(identifierOrTemplateChain);
            }
        }
    }

    void format(const Module module_)
    {
        debug(verbose) writeln("Module");

        /**
        ModuleDeclaration moduleDeclaration;
        Declaration[] declarations;
        **/

        format(module_.moduleDeclaration);
        foreach(decl; module_.declarations)
            format(decl);
    }

    void format(const ModuleDeclaration moduleDeclaration)
    {
        debug(verbose) writeln("ModuleDeclaration");
        if (moduleDeclaration is null) return;

        /**
        IdentifierChain moduleName;
        **/

        put("module ");
        format(moduleDeclaration.moduleName);
        put(";");
        newlineIndent();
        newlineIndent();
    }

    void format(const MulExpression mulExpression)
    {
        debug(verbose) writeln("MulExpression");
        mixin(binary("mulExpression"));
    }

    void format(const NewAnonClassExpression newAnonClassExpression)
    {
        debug(verbose) writeln("NewAnonClassExpression");

        /**
        Arguments allocatorArguments;
        Arguments constructorArguments;
        BaseClassList baseClassList;
        StructBody structBody;
        **/

        with(newAnonClassExpression)
        {
            if (allocatorArguments)
            {
                format(allocatorArguments);
                space();
            }
            put("class");
            if (constructorArguments)
                format(constructorArguments);

            if (baseClassList)
            {
                space();
                format(baseClassList);
            }

            format(structBody);
        }
    }

    void format(const NewExpression newExpression)
    {
        debug(verbose) writeln("NewExpression");

        /**
        Type type;
        NewAnonClassExpression newAnonClassExpression;
        Arguments arguments;
        AssignExpression assignExpression;
        **/

        with(newExpression)
        {
            put("new ");
            if (newAnonClassExpression) format(newAnonClassExpression);
            else
            {
                if (type) format(type);
                if (arguments) format(arguments);
                if (assignExpression)
                {
                    put("[");
                    format(assignExpression);
                    put("]");
                }
            }
        }
    }

    void format(const NonVoidInitializer nonVoidInitializer)
    {
        debug(verbose) writeln("NonVoidInitializer");

        /**
        AssignExpression assignExpression;
        ArrayInitializer arrayInitializer;
        StructInitializer structInitializer;
        **/

        with(nonVoidInitializer)
        {
            if (assignExpression) format(assignExpression);
            else if (arrayInitializer) format(arrayInitializer);
            else if (structInitializer) format(structInitializer);
        }
    }

    void format(const Operands operands)
    {
        debug(verbose) writeln("Operands");
        assert(false);
    }

    void format(const OrExpression orExpression)
    {
        debug(verbose) writeln("OrExpression");
        mixin(binary("orExpression", "|"));
    }

    void format(const OrOrExpression orOrExpression)
    {
        debug(verbose) writeln("OrOrExpression");
        mixin(binary("orOrExpression", "||"));
    }

    void format(const OutContractExpression expression)
    {
        debug(verbose) writeln("OutContractExpression");
        newlineIndent();
        put("out (");
        if (expression.parameter != tok!"")
            format(expression.parameter);
        put("; ");
        format(expression.assertArguments);
        put(")");
    }

    void format(const OutStatement stmnt)
    {
        debug(verbose) writeln("OutStatement");

        /**
        Token parameter;
        BlockStatement blockStatement;
        **/
        newlineIndent();
        put("out");
        if (stmnt.parameter != tok!"")
        {
            put(" (");
            format(stmnt.parameter);
            put(")");
        }
        format(stmnt.blockStatement);
    }

    void format(const ParameterAttribute pa)
    {
        debug(verbose) writeln("ParameterAttribute");

        if (pa.atAttribute)
            format(pa.atAttribute);
        else
            put(tokenRep(pa.idType));
    }

    void format(const Parameter parameter)
    {
        debug(verbose) writeln("Parameter");

        /**
        ParameterAttribute[] parameterAttributes;
        Type type;
        Token name;
        bool vararg;
        AssignExpression default_;
        TypeSuffix[] cstyle;
        **/

        foreach (count, attribute; parameter.parameterAttributes)
        {
            if (count) space();
            format(attribute);
        }

        if (parameter.parameterAttributes.length > 0)
            space();

        if (parameter.type !is null)
            format(parameter.type);

        if (parameter.name.type != tok!"")
        {
            space();
            put(parameter.name.text);
        }

        foreach(suffix; parameter.cstyle)
            format(suffix);

        if (parameter.default_)
        {
            put(" = ");
            format(parameter.default_);
        }

        if (parameter.vararg)
            put("...");
    }

    void format(const Parameters parameters)
    {
        debug(verbose) writeln("Parameters");

        /**
        Parameter[] parameters;
        bool hasVarargs;
        **/

        put("(");
        foreach (count, param; parameters.parameters)
        {
            if (count) put(", ");
            format(param);
        }
        if (parameters.hasVarargs)
        {
            if (parameters.parameters.length)
                put(", ");
            put("...");
        }
        put(")");
    }

    void format(const Postblit postblit, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("Postblit");

        /**
        FunctionBody functionBody;
        **/

        newThing(What.functionDecl);
        putAttrs(attrs);
        put("this(this)");

        foreach(attr; postblit.memberFunctionAttributes)
        {
            space();
            format(attr);
        }

        if (postblit.functionBody)
            format(postblit.functionBody);
        else
            put(";");
    }

    void format(const PowExpression powExpression)
    {
        debug(verbose) writeln("PowExpression");
        mixin(binary("powExpression", "^^", true));
    }

    void format(const PragmaDeclaration pragmaDeclaration, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("PragmaDeclaration");

        /**
        PragmaExpression pragmaExpression;
        **/

        putAttrs(attrs);
        format(pragmaDeclaration.pragmaExpression);
        put(";");
    }

    void format(const PragmaExpression pragmaExpression)
    {
        debug(verbose) writeln("PragmaExpression");

        /**
        Token identifier;
        ArgumentList argumentList;
        **/

        put("pragma(");
        format(pragmaExpression.identifier);
        if (pragmaExpression.argumentList)
        {
            put(", ");
            format(pragmaExpression.argumentList);
        }
        put(")");
    }

    void format(const PrimaryExpression primaryExpression)
    {
        debug(verbose) writeln("PrimaryExpression");

        /**
        Token dot;
        Token primary;
        IdentifierOrTemplateInstance identifierOrTemplateInstance;
        Token basicType;
        TypeofExpression typeofExpression;
        TypeidExpression typeidExpression;
        ArrayLiteral arrayLiteral;
        AssocArrayLiteral assocArrayLiteral;
        Expression expression;
        IsExpression isExpression;
        FunctionLiteralExpression functionLiteralExpression;
        TraitsExpression traitsExpression;
        MixinExpression mixinExpression;
        ImportExpression importExpression;
        Vector vector;
        Type type;
        Token typeConstructor;
        Arguments arguments;
        **/

        with(primaryExpression)
        {
            if (dot != tok!"") put(".");
            if (basicType != tok!"") format(basicType);
            if (primary != tok!"")
            {
                if (basicType != tok!"") put("."); // i.e. : uint.max
                format(primary);
            }

            if (expression)
            {
                put("(");
                format(expression);
                put(")");
            }
            else if (identifierOrTemplateInstance)
            {
                format(identifierOrTemplateInstance);
            }
            else if (typeofExpression) format(typeofExpression);
            else if (typeidExpression) format(typeidExpression);
            else if (arrayLiteral) format(arrayLiteral);
            else if (assocArrayLiteral) format(assocArrayLiteral);
            else if (isExpression) format(isExpression);
            else if (functionLiteralExpression) format(functionLiteralExpression);
            else if (traitsExpression) format(traitsExpression);
            else if (mixinExpression) format(mixinExpression);
            else if (importExpression) format(importExpression);
            else if (vector) format(vector);
            else if (type) format(type);
            else if (arguments) format(arguments);
        }
    }

    void format(const Register register)
    {
        debug(verbose) writeln("Register");
        assert(false);
    }

    void format(const RelExpression relExpression)
    {
        debug(verbose) writeln("RelExpression");
        mixin(binary("relExpression"));
    }

    void format(const ReturnStatement returnStatement)
    {
        debug(verbose) writeln("ReturnStatement");

        put("return");
        if (returnStatement.expression)
        {
            space();
            format(returnStatement.expression);
        }
        put(";");
    }

    void format(const ScopeGuardStatement scopeGuardStatement)
    {
        debug(verbose) writeln("ScopeGuardStatement");

        /**
        Token identifier;
        StatementNoCaseNoDefault statementNoCaseNoDefault;
        **/

        with(scopeGuardStatement)
        {
            put("scope(");
            format(identifier);
            put(")");
            indent();
            format(statementNoCaseNoDefault);
            outdent();
        }
    }

    void format(const SharedStaticConstructor sharedStaticConstructor, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("SharedStaticConstructor");

        with(sharedStaticConstructor)
        {
            newThing(What.functionDecl);
            putComment(comment);
            putAttrs(attrs);
            put("shared static this()");
            format(functionBody);
        }
    }

    void format(const SharedStaticDestructor sharedStaticDestructor, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("SharedStaticDestructor");

        with(sharedStaticDestructor)
        {
            newThing(What.functionDecl);
            putComment(comment);
            putAttrs(attrs);
            put("shared static ~this()");
            format(functionBody);
        }
    }

    void format(const ShiftExpression shiftExpression)
    {
        debug(verbose) writeln("ShiftExpression");
        mixin(binary("shiftExpression"));
    }

    void format(const ShortenedFunctionBody shortenedFunctionBody)
    {
        debug(verbose) writeln("ShortenedFunctionBody");
        put("=> ");
        format(shortenedFunctionBody.expression);
        put(";");
    }

    void format(const SingleImport singleImport)
    {
        debug(verbose) writeln("SingleImport");

        /**
        Token rename;
        IdentifierChain identifierChain;
        **/

        if (singleImport.rename != tok!"")
        {
            format(singleImport.rename);
            put(" = ");
        }
        format(singleImport.identifierChain);
    }

    void format(const SpecifiedFunctionBody specifiedFunctionBody)
    {
        debug(verbose) writeln("SpecifiedFunctionBody");

        with(specifiedFunctionBody)
        {
            foreach (contract; functionContracts)
                format(contract);
            if (specifiedFunctionBody.hasDo)
            {
                newlineIndent();
                put("do");
            }
            if (blockStatement)
                format(blockStatement);
        }
    }

    void format(const Statement statement)
    {
        debug(verbose) writeln("Statement");

        /**
        StatementNoCaseNoDefault statementNoCaseNoDefault;
        CaseStatement caseStatement;
        CaseRangeStatement caseRangeStatement;
        DefaultStatement defaultStatement;
        **/

        with(statement)
        {
            if (statementNoCaseNoDefault)
            {
                format(statementNoCaseNoDefault);
                return;
            }

            newlineIndent();
            if (caseStatement) format(caseStatement);
            else if (caseRangeStatement) format(caseRangeStatement);
            else if (defaultStatement) format(defaultStatement);
        }
    }

    void format(const StatementNoCaseNoDefault statementNoCaseNoDefault)
    {
        debug(verbose) writeln("StatementNoCaseNoDefault");

        string mix(string s) { return "if (" ~ s ~ ") format(" ~ s ~ ");"; }

        with(statementNoCaseNoDefault)
        {
            if (!blockStatement) newlineIndent();

            enum stmnts = TypeTuple!(
                "labeledStatement",
                "blockStatement",
                "ifStatement",
                "whileStatement",
                "doStatement",
                "forStatement",
                "foreachStatement",
                "switchStatement",
                "finalSwitchStatement",
                "continueStatement",
                "breakStatement",
                "returnStatement",
                "gotoStatement",
                "withStatement",
                "synchronizedStatement",
                "tryStatement",
                "scopeGuardStatement",
                "asmStatement",
                "conditionalStatement",
                "staticAssertStatement",
                "versionSpecification",
                "debugSpecification",
                "expressionStatement"
            );

            foreach(s; stmnts)
                mixin(mix(s));
        }
    }

    void format(const StaticAssertDeclaration staticAssertDeclaration, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("StaticAssertDeclaration");

        newThing(What.other);
        putAttrs(attrs);
        format(staticAssertDeclaration.staticAssertStatement);
        put(";");
    }

    void format(const StaticAssertStatement staticAssertStatement)
    {
        debug(verbose) writeln("StaticAssertStatement");

        put("static ");
        format(staticAssertStatement.assertExpression);
    }

    void format(const StaticConstructor staticConstructor, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("StaticConstructor");

        putAttrs(attrs);
        put("static this()");
        format(staticConstructor.functionBody);
    }

    void format(const StaticDestructor staticDestructor, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("StaticDestructor");

        putAttrs(attrs);
        put("static ~this()");
        format(staticDestructor.functionBody);
    }

    void format(const StaticIfCondition staticIfCondition)
    {
        debug(verbose) writeln("StaticIfCondition");

        put("static if (");
        format(staticIfCondition.assignExpression);
        put(")");
    }

    void format(const StorageClass storageClass)
    {
        debug(verbose) writeln("StorageClass");

        /**
        AtAttribute atAttribute;
        Deprecated deprecated_;
        LinkageAttribute linkageAttribute;
        Token token;
        **/

        with(storageClass)
        {
            if (atAttribute) format(atAttribute);
            else if (deprecated_) format(deprecated_);
            else if (linkageAttribute) format(linkageAttribute);
            else format(token);
        }
    }

    void format(const StructBody structBody)
    {
        debug(verbose) writeln("StructBody");

        if (structBody.declarations.length > 0)
        {
            startBlock();
            foreach(count, decl; structBody.declarations)
                format(decl);
            endBlock();
        }
        else
        {
            space();
            put("{}");
        }
    }

    void format(const StructDeclaration decl, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("StructDeclaration");

        /**
        Token name;
        TemplateParameters templateParameters;
        Constraint constraint;
        StructBody structBody;
        string comment;
        **/

        newThing(What.aggregateDecl);
        putComment(decl.comment);
        putAttrs(attrs);
        put("struct ");
        format(decl.name);

        if (decl.templateParameters)
            format(decl.templateParameters);

        if (decl.constraint)
        {
            space();
            format(decl.constraint);
        }

        if (decl.structBody)
            format(decl.structBody);
        else
            put(";");
    }

    void format(const StructInitializer structInitializer)
    {
        debug(verbose) writeln("StructInitializer");

        put("{");
        format(structInitializer.structMemberInitializers);
        put("}");
    }

    void format(const StructMemberInitializer structMemberInitializer)
    {
        debug(verbose) writeln("StructMemberInitializer");

        /**
        Token identifier;
        NonVoidInitializer nonVoidInitializer;
        **/

        with(structMemberInitializer)
        {
            if (identifier != tok!"")
            {
                format(identifier);
                put(":");
            }
            format(nonVoidInitializer);
        }
    }

    void format(const StructMemberInitializers structMemberInitializers)
    {
        debug(verbose) writeln("StructMemberInitializers");

        foreach(count, mem; structMemberInitializers.structMemberInitializers)
        {
            if (count) put(", ");
            format(mem);
        }
    }

    void format(const SwitchStatement switchStatement, bool isFinal = false)
    {
        debug(verbose) writeln("SwitchStatement");

        /**
        Expression expression;
        Statement statement;
        **/

        with(switchStatement)
        {
            newThing(What.other);
            isFinal ? put(" final switch(") : put("switch(");
            format(expression);
            put(")");

            bool needBlock = statement.statementNoCaseNoDefault &&
                             !(statement.statementNoCaseNoDefault.withStatement ||
                               statement.statementNoCaseNoDefault.blockStatement );

            if (needBlock)
                startBlock();
            format(statement);
            if (needBlock)
                endBlock();
        }
    }

    void format(const Symbol symbol)
    {
        debug(verbose) writeln("Symbol");

        if (symbol.dot)
            put(".");
        format(symbol.identifierOrTemplateChain);
    }

    void format(const SynchronizedStatement synchronizedStatement)
    {
        debug(verbose) writeln("SynchronizedStatement");

        /**
        Expression expression;
        StatementNoCaseNoDefault statementNoCaseNoDefault;
        **/

        with(synchronizedStatement)
        {
            put("synchronized");
            if (expression)
            {
                put("(");
                format(expression);
                put(")");
            }
            format(statementNoCaseNoDefault);
        }
    }

    void format(const TemplateAliasParameter templateAliasParameter)
    {
        debug(verbose) writeln("TemplateAliasParameter");

        /**
        Type type;
        Token identifier;
        Type colonType;
        AssignExpression colonExpression;
        Type assignType;
        AssignExpression assignExpression;
        **/

        with(templateAliasParameter)
        {
            put("alias ");
            if (type)
            {
                format(type);
                space();
            }
            format(identifier);
            if (colonType)
            {
                put(" : ");
                format(colonType);
            }
            else if (colonExpression)
            {
                put(" : ");
                format(colonExpression);
            }
            if (assignType)
            {
                put(" = ");
                format(assignType);
            }
            else if (assignExpression)
            {
                put(" = ");
                format(assignExpression);
            }
        }
    }

    void format(const TemplateArgument templateArgument)
    {
        debug(verbose) writeln("TemplateArgument");

        /**
        Type type;
        AssignExpression assignExpression;
        **/

        with(templateArgument)
        {
            if (type) format(type);
            if (assignExpression) format(assignExpression);
        }
    }

    void format(const TemplateArgumentList templateArgumentList, bool parens = true)
    {
        debug(verbose) writeln("TemplateArgumentList");

        if (parens) put("!(");
        foreach(count, arg; templateArgumentList.items)
        {
            if (count) put(", ");
            format(arg);
        }
        if (parens) put(")");
    }

    void format(const TemplateArguments templateArguments)
    {
        debug(verbose) writeln("TemplateArguments");

        /**
        TemplateArgumentList templateArgumentList;
        TemplateSingleArgument templateSingleArgument;
        **/

        with(templateArguments)
        {
            if (templateArgumentList) format(templateArgumentList);
            else if (templateSingleArgument) format(templateSingleArgument);
            else put("!()");
        }
    }

    void format(const TemplateDeclaration templateDeclaration, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("TemplateDeclaration");

        /**
        Token name;
        TemplateParameters templateParameters;
        Constraint constraint;
        Declaration[] declarations;
        EponymousTemplateDeclaration eponymousTemplateDeclaration;
        string comment;
        **/

        with(templateDeclaration)
        {
            newThing(What.other);
            putComment(comment);
            putAttrs(attrs);

            put("template ");
            format(name);

            if (templateParameters)
                format(templateParameters);

            if (constraint)
            {
                space();
                format(constraint);
            }

            startBlock();
            foreach(d; declarations)
                format(d);
            endBlock();
        }
    }

    void format(const TemplateInstance templateInstance)
    {
        debug(verbose) writeln("TemplateInstance");

        /**
        Token identifier;
        TemplateArguments templateArguments;
        **/

        with(templateInstance)
        {
            format(identifier);
            if (templateArguments) format(templateArguments);
        }
    }

    void format(const TemplateMixinExpression templateMixinExpression)
    {
        debug(verbose) writeln("TemplateMixinExpression");

        /**
        Token identifier;
        TemplateArguments templateArguments;
        MixinTemplateName mixinTemplateName;
        **/

        with(templateMixinExpression)
        {
            put("mixin ");
            format(mixinTemplateName);
            if (templateArguments) format(templateArguments);
            space();
            format(identifier);
        }
    }

    void format(const TemplateParameter templateParameter)
    {
        debug(verbose) writeln("TemplateParameter");

        with(templateParameter)
        {
            if (templateTypeParameter)
                format(templateTypeParameter);
            else if (templateValueParameter)
                format(templateValueParameter);
            else if (templateAliasParameter)
                format(templateAliasParameter);
            else if (templateTupleParameter)
                format(templateTupleParameter);
            else if (templateThisParameter)
                format(templateThisParameter);
        }
    }

    void format(const TemplateParameterList templateParameterList)
    {
        debug(verbose) writeln("TemplateParameterList");

        foreach(i, param; templateParameterList.items)
        {
            if (i) put(", ");
            format(param);
        }
    }

    void format(const TemplateParameters templateParameters)
    {
        debug(verbose) writeln("TemplateParameters");

        with(templateParameters)
        {
            put("(");
            if (templateParameterList)
                format(templateParameterList);
            put(")");
        }
    }

    void format(const TemplateSingleArgument templateSingleArgument)
    {
        debug(verbose) writeln("TemplateSingleArgument");

        /**
        Token token;
        **/

        put("!");
        format(templateSingleArgument.token);
    }

    void format(const TemplateThisParameter templateThisParameter)
    {
        debug(verbose) writeln("TemplateThisParameter");

        with(templateThisParameter)
        {
            put("this ");
            if (templateTypeParameter)
                format(templateTypeParameter);
        }
    }

    void format(const TemplateTupleParameter templateTupleParameter)
    {
        debug(verbose) writeln("TemplateTupleParameter");

        format(templateTupleParameter.identifier);
        put("...");
    }

    void format(const TemplateTypeParameter templateTypeParameter)
    {
        debug(verbose) writeln("TemplateTypeParameter");

        /**
        Token identifier;
        Type colonType;
        Type assignType;
        **/

        with(templateTypeParameter)
        {
            format(identifier);
            if (colonType)
            {
                put(" : ");
                format(colonType);
            }
            if (assignType)
            {
                put(" = ");
                format(assignType);
            }
        }
    }

    void format(const TemplateValueParameter templateValueParameter)
    {
        debug(verbose) writeln("TemplateValueParameter");

        /**
        Type type;
        Token identifier;
        Expression expression;
        TemplateValueParameterDefault templateValueParameterDefault;
        **/

        with(templateValueParameter)
        {
            if (type) format(type);
            space();
            format(identifier);

            if (assignExpression)
            {
                put(" : ");
                format(assignExpression);
            }

            if (templateValueParameterDefault)
            {
                put(" = ");
                format(templateValueParameterDefault);
            }
        }
    }

    void format(const TemplateValueParameterDefault templateValueParameterDefault)
    {
        debug(verbose) writeln("TemplateValueParameterDefault");

        with(templateValueParameterDefault)
            assignExpression ? format(assignExpression) : format(token);
    }

    void format(const TernaryExpression expr)
    {
        debug(verbose) writeln("TernaryExpression");

        /**
        ExpressionNode orOrExpression;
        ExpressionNode expression;
        ExpressionNode ternaryExpression;
        **/

        format(expr.orOrExpression);

        if (expr.expression && expr.ternaryExpression)
        {
            put(" ? ");
            format(expr.expression);
            put(" : ");
            format(expr.ternaryExpression);
        }
    }

    void format(const ThrowExpression throwExpression)
    {
        debug(verbose) writeln("ThrowExpression");

        put("throw ");
        assert(throwExpression.expression);
        format(throwExpression.expression);
    }

    void format(const Token token)
    {
        debug(verbose) writeln("Token ", tokenRep(token));
        put(tokenRep(token));
    }

    void format(const TraitsExpression traitExpr)
    {
        debug(verbose) writeln("TraitsExpression");

        /**
        Token identifier;
        TemplateArgumentList templateArgumentList;
        **/

        put("__traits(");
        format(traitExpr.identifier);
        put(", ");
        format(traitExpr.templateArgumentList, false);
        put(")");
    }

    void format(const TryStatement tryStatement)
    {
        debug(verbose) writeln("TryStatement");

        /**
        DeclarationOrStatement declarationOrStatement;
        Catches catches;
        Finally finally_;
        **/

        with(tryStatement)
        {
            newThing(What.other);
            put("try");
            maybeIndent(declarationOrStatement);
            if (catches) format(catches);
            if (finally_) format(finally_);
        }
    }

    void format(const Type type)
    {
        debug(verbose) writeln("Type(");

        /**
        IdType[] typeConstructors;
        TypeSuffix[] typeSuffixes;
        Type2 type2;
        **/

        foreach (count, constructor; type.typeConstructors)
        {
            if (count) space();
            put(tokenRep(constructor));
        }

        if (type.typeConstructors.length) space();
        if (type.type2) format(type.type2);

        foreach (suffix; type.typeSuffixes)
            format(suffix);

        debug(verbose) writeln(")");
    }

    void format(const Type2 type2)
    {
        debug(verbose) writeln("Type2");

        /**
        IdType builtinType;
        TypeofExpression typeofExpression;
        IdentifierList identifierList;
        IdType typeConstructor;
        Type type;
        TraitsExpression traitsExpression;
        MixinExpression mixinExpression;
        **/

        if (type2.typeIdentifierPart !is null)
        {
            format(type2.typeIdentifierPart);
        }
        else if (type2.typeofExpression !is null)
        {
            format(type2.typeofExpression);
            if (type2.typeIdentifierPart)
            {
                put(".");
                format(type2.typeIdentifierPart);
            }
            return;
        }
        else if (type2.typeConstructor != tok!"")
        {
            put(tokenRep(type2.typeConstructor));
            put("(");
            format(type2.type);
            put(")");
        }
        else if (type2.traitsExpression)
        {
            format(type2.traitsExpression);
        }
        else if (type2.mixinExpression)
        {
            format(type2.mixinExpression);
        }
        else
        {
            put(tokenRep(type2.builtinType));
            if (type2.typeIdentifierPart)
            {
                put(".");
                format(type2.typeIdentifierPart);
            }
        }
    }

    void format(const TypeSpecialization typeSpecialization)
    {
        debug(verbose) writeln("TypeSpecialization");

        /**
        Token token;
        Type type;
        **/

        with(typeSpecialization)
        {
            format(token);
            if (type) format(type);
        }
    }

    void format(const TypeSuffix typeSuffix)
    {
        debug(verbose) writeln("TypeSuffix");

        /**
        Token delegateOrFunction;
        bool star;
        bool array;
        Type type;
        AssignExpression low;
        AssignExpression high;
        Parameters parameters;
        MemberFunctionAttribute[] memberFunctionAttributes;
        **/

        if (typeSuffix.star.type != tok!"")
        {
            put("*");
            return;
        }
        else if (typeSuffix.array)
        {
            if (typeSuffix.type is null)
            {
                if (typeSuffix.low is null)
                {
                    put("[]");
                    return;
                }
                else
                {
                    if (typeSuffix.high is null)
                    {
                        put("[");
                        format(typeSuffix.low);
                        put("]");
                        return;
                    }
                    else
                    {
                        put("[");
                        format(typeSuffix.low);
                        put("..");
                        format(typeSuffix.high);
                        put("]");
                        return;
                    }
                }
            }
            else
            {
                put("[");
                format(typeSuffix.type);
                put("]");
                return;
            }
        }
        else
        {
            space();
            format(typeSuffix.delegateOrFunction);
            if (typeSuffix.parameters) format(typeSuffix.parameters);
            foreach(attr; typeSuffix.memberFunctionAttributes)
            {
                space();
                format(attr);
            }
            return;
        }
    }

    void format(const TypeidExpression idExpr)
    {
        debug(verbose) writeln("TypeidExpression");

        /**
        Type type;
        Expression expression;
        **/

        put("typeid (");
        idExpr.type ? format(idExpr.type) : format(idExpr.expression);
        put(")");
    }

    void format(const TypeofExpression typeofExpr)
    {
        debug(verbose) writeln("TypeofExpression");

        /**
        Expression expression;
        Token return_;
        **/

        put("typeof(");
        typeofExpr.expression ? format(typeofExpr.expression) : format(typeofExpr.return_);
        put(")");
    }

    void format(const UnaryExpression unary)
    {
        debug(verbose) writeln("UnaryExpression(");

        /**
        Type type;
        PrimaryExpression primaryExpression;
        Token prefix;
        Token suffix;
        UnaryExpression unaryExpression;
        NewExpression newExpression;
        DeleteExpression deleteExpression;
        CastExpression castExpression;
        FunctionCallExpression functionCallExpression;
        ArgumentList argumentList;
        IdentifierOrTemplateInstance identifierOrTemplateInstance;
        AssertExpression assertExpression;
        SliceExpression sliceExpression;
        IndexExpression indexExpression;
        **/

        with(unary)
        {
            if (prefix != tok!"") format(prefix);

            if (type)
            {
                // handle things like (void*).sizeof
                if (identifierOrTemplateInstance)
                {
                    put("(");
                    format(type);
                    put(")");
                }
                else
                {
                    format(type);
                    put("(");
                    if (argumentList)
                        format(argumentList);
                    put(")");
                }
            }

            if (primaryExpression) format(primaryExpression);
            if (newExpression) format(newExpression);
            if (deleteExpression) format(deleteExpression);
            if (castExpression) format(castExpression);
            if (functionCallExpression) format(functionCallExpression);
            if (assertExpression) format(assertExpression);
            if (throwExpression) format(throwExpression);
            if (indexExpression) format(indexExpression);

            if (unaryExpression) format(unaryExpression);
            if (suffix != tok!"") format(suffix);

            if (identifierOrTemplateInstance)
            {
                put(".");
                format(identifierOrTemplateInstance);
            }
        }

        debug(verbose) writeln(")");
    }

    void format(const UnionDeclaration decl, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("UnionDeclaration");

        /**
        Token name;
        TemplateParameters templateParameters;
        Constraint constraint;
        StructBody structBody;
        string comment;
        **/

        newThing(What.aggregateDecl);
        putComment(decl.comment);
        putAttrs(attrs);
        put("union ");
        format(decl.name);
        if (decl.templateParameters)
            format(decl.templateParameters);
        if (decl.constraint)
        {
            space();
            format(decl.constraint);
        }
        format(decl.structBody);
    }

    void format(const Unittest unittest_, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("Unittest");

        /**
        BlockStatement blockStatement;
        string comment;
        **/

        newThing(What.functionDecl);
        putComment(unittest_.comment);
        putAttrs(attrs);
        put("unittest");
        format(unittest_.blockStatement);
    }

    void format(const VariableDeclaration decl, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("VariableDeclaration");

        /**
        Type type;
        Declarator[] declarators;
        StorageClass storageClass;
        AutoDeclaration autoDeclaration;
        string comment;
        **/

        newThing(What.variableDecl);
        putComment(decl.comment);
        putAttrs(attrs);

        if (decl.autoDeclaration)
            format(decl.autoDeclaration);
        else
        {
            foreach (c; decl.storageClasses)
            {
                format(c);
                space();
            }
            if (decl.type) format(decl.type);
            if (decl.declarators.length) space();
            foreach(count, d; decl.declarators)
            {
                if (count) put(", ");
                format(d);
            }
        }
        put(";");
    }

    void format(const Vector vector)
    {
        debug(verbose) writeln("Vector");

        put("__vector");
        if (vector.type)
        {
            put("(");
            format(vector.type);
            put(")");
        }
    }

    void format(const VersionCondition versionCondition)
    {
        debug(verbose) writeln("VersionCondition");

        put("version (");
        format(versionCondition.token);
        put(")");
    }

    void format(const VersionSpecification ver, const Attribute[] attrs = null)
    {
        debug(verbose) writeln("VersionSpecification");

        newThing(What.other);
        putAttrs(attrs);
        put("version = ");
        format(ver.token);
        put(";");
    }

    void format(const WhileStatement stmt)
    {
        debug(verbose) writeln("WhileStatement");

        /**
        Expression expression;
        DeclarationOrStatement declarationOrStatement;
        **/

        newThing(What.other);
        put("while (");
        format(stmt.expression);
        put(")");
        maybeIndent(stmt.declarationOrStatement);
    }

    void format(const WithStatement stmt)
    {
        debug(verbose) writeln("WithStatement");

        /**
        Expression expression;
        DeclarationsOrStatement declarationOrStatement;
        **/

        space();
        put("with (");
        format(stmt.expression);
        put(")");
        if (stmt.declarationOrStatement)
            format(stmt.declarationOrStatement);
    }

    void format(const XorExpression xorExpression)
    {
        debug(verbose) writeln("XorExpression");
        mixin(binary("xorExpression", "^"));
    }

    Sink sink;

protected:

    import std.uni : isWhite;

    void indent()
    {
        indentLevel++;
    }

    void outdent()
    {
        if (indentLevel == 0)
            return;
        indentLevel--;
    }

    void putIndent()
    {
        if (!indentLevel) return;
        auto i = getIndent();
        put(i);
    }

    string getIndent()
    {
        if (useTabs)
        {
            char[] c = new char[indentLevel];
            c[] = '\t';
            return cast(string) c;
        }
        else
        {
            char[] c = new char[indentLevel * indentWidth];
            c[] = ' ';
            return cast(string) c;
        }
    }

    enum What
    {
        functionDecl,
        aggregateDecl,
        attributeDecl,
        conditionalDecl,
        variableDecl,
        importDecl,
        expr,
        loop,
        else_,
        catch_,
        other
    }

    void newThing(What thing)
    {
        lastThing = currentThing;
        currentThing = thing;

        with(What) {

            if (lastThing == importDecl && thing != importDecl)
            {
                lineGap(1);
                return;
            }

            if (lastThing == loop)
            {
                lineGap(1);
                return;
            }

            switch(thing)
            {
                case other:
                    newline();
                    break;
                case aggregateDecl:
                case attributeDecl:
                case functionDecl:
                    lineGap(1);
                    break;
                case conditionalDecl:
                    //lineGap(1);
                    break;
                case variableDecl:
                    lineGap(1);
                    break;
                case importDecl:
                    newlineIndent();
                    break;
                case expr: break;
                case catch_: goto case;
                case else_:
                    final switch(style) with(IndentStyle)
                    {
                        case allman: newline(); break;
                        case otbs: space(); break;
                    }
                    break;
                default: newlineIndent(); break;
            }
        }
    }

    void lineGap(int gap)
    {
        foreach (i; 0 .. gap + 1)
        {
            if (i == gap)
                newlineIndent();
            else
                newline();
        }
    }

    void newlineIndent()
    {
        if (ignoreNewlines)
        {
            space(); // don't do this when splitting lines
        }
        else
        {
            sink.put("\n");
            lineLength = 0;
            putIndent();
        }
    }

    void newline()
    {
        sink.put("\n");
        lineLength = 0;
    }

    void space()
    {
        put(" ");
    }

    static string binary(string symbol, string operator = null, bool nospace = false)
    {
        return "with(" ~ symbol ~ "){"
             ~ "format(left); if (right is null) return;"
             ~ (nospace ? "" : "put(` `);")
             ~ (operator ? "put(`" ~ operator ~ "`);" : "put(tokenRep(operator));")
             ~ (nospace ? "" : "put(` `);")
             ~ "format(right);}";
    }

    void startBlock()
    {
        final switch(style) with(IndentStyle)
        {
            case allman: newline(); break;
            case otbs: space(); break;
        }
        putIndent();
        put("{");
        indent();
    }

    void endBlock()
    {
        outdent();
        newline();
        putIndent();
        put("}");
    }

    string tokenRep(Token t)
    {
        return t.text.length ? t.text : tokenRep(t.type);
    }

    string tokenRep(IdType t)
    {
        return t ? str(t) : "";
    }

    void putComment(string c)
    {
        import std.string : splitLines;
        if (!c.length) return;
        put(c.splitLines().join("\n" ~ getIndent()));
        newlineIndent();
    }

    void putAttrs(const Attribute[] attrs)
    {
        if (attrs !is null)
        {
            foreach(count, attr; attrs)
            {
                format(attr);
                space();
            }
        }
    }

    void put(string s)
    {
        sink.put(s);
        lineLength += s.length; // TODO: tabs / spaces?
    }

    void formatCaseDecls(const DeclarationsAndStatements declsAndStmnts)
    {
        bool seenBlock = false;
        auto items = declsAndStmnts.declarationsAndStatements;
        foreach(item; items)
        {
            bool _indent = false;
            if (item.declaration) _indent = true;
            if (item.statement && item.statement.statementNoCaseNoDefault)
            {
                if (item.statement.statementNoCaseNoDefault.blockStatement)
                    seenBlock = true;
                else if (!item.statement.statementNoCaseNoDefault.labeledStatement)
                    _indent = true;
            }
            if (seenBlock) _indent = false;
            if (_indent) indent();
            format(item);
            if (_indent) outdent();
        }
    }

    bool needIndent(const Statement s)
    {
        return s.statementNoCaseNoDefault &&
               !s.statementNoCaseNoDefault.blockStatement;
    }

    bool needIndent(const Declaration d)
    {
        return !d.declarations.length;
    }

    bool needIndent(const DeclarationOrStatement dors)
    {
        return (dors.declaration && needIndent(dors.declaration)) ||
               (dors.statement && needIndent(dors.statement));
    }

    void maybeIndent(T)(const T t)
    {
        auto _indent = needIndent(t);
        if (_indent) indent();
        format(t);
        if (_indent) outdent();
    }

    bool isEmptyDeclaration(const Declaration decl)
    {
        with(decl)
        {
            string mix(string[] s) {
                string r;
                foreach(c, d; s)
                    r ~= (c > 0 ? "else " : "") ~ "if (" ~ d ~ ") return false;";
                return r;
            }
            mixin(mix(possibleDeclarations));
            return attributes.length == 0 &&
                   declarations.length == 0;
        }
    }

    bool ignoreNewlines = false;
    bool useTabs;
    uint caseDepth;
    uint indentWidth;
    uint indentLevel;
    IndentStyle style;


    What lastThing, currentThing;
    uint lineLength;
    uint maxLineLength = 80;

    enum possibleDeclarations = [
        "attributeDeclaration",
        "importDeclaration",
        "functionDeclaration",
        "variableDeclaration",
        "aliasThisDeclaration",
        "structDeclaration",
        "classDeclaration",
        "interfaceDeclaration",
        "unionDeclaration",
        "enumDeclaration",
        "aliasDeclaration",
        "mixinDeclaration",
        "mixinTemplateDeclaration",
        "unittest_",
        "staticAssertDeclaration",
        "templateDeclaration",
        "constructor",
        "destructor",
        "staticConstructor",
        "staticDestructor",
        "sharedStaticDestructor",
        "sharedStaticConstructor",
        "conditionalDeclaration",
        "pragmaDeclaration",
        "versionSpecification",
        "invariant_",
        "postblit"
    ];
}

version (unittest)
void testFormatNode(Node)(string sourceCode, string expected = "")
{
    Appender!string fmt;
    ubyte[] code = cast(ubyte[]) sourceCode;

    class CatchInterestingStuff : ASTVisitor
    {
        alias visit = ASTVisitor.visit;
        override void visit(const(Node) stuff)
        {
            stuff.accept(this);
            format(&fmt, stuff);
            const exp = expected.length ? expected : sourceCode;
            const act = fmt.data();
            if (!act.canFind(exp))
            {
                string msg = "Formatted code didn't contain the expected output!";
                msg ~= "\n=============== Expected ===================\n";
                msg ~= exp;
                msg ~= "\n=============== Actual ===================\n";
                msg ~= act;
                msg ~= "\n==========================================";
                assert(false, msg);
            }
        }
    }

    static void errorFunc(string,size_t,size_t,string msg, bool)
    {assert(0, "The AST to format contains error(s) : " ~ msg);}

    LexerConfig config;
    StringCache cache = StringCache(32);
    RollbackAllocator rba;
    auto toks = getTokensForParser(code, config, &cache);
    Module mod = parseModule(ParserConfig(toks, "stdin", &rba, &errorFunc));
    (new CatchInterestingStuff).visit(mod);
}

unittest
{
    testFormatNode!(VariableDeclaration)("__traits(getMember, Foo, \"Bar\") fooBar;");
    testFormatNode!(VariableDeclaration)("const(__traits(getMember, Foo, \"Bar\")) fooBar;");
    testFormatNode!(VariableDeclaration)("T.T y;");
    testFormatNode!(VariableDeclaration)("T[] y;");
    testFormatNode!(VariableDeclaration)("T* y;");
    testFormatNode!(VariableDeclaration)("T[0].Y y;");
    testFormatNode!(VariableDeclaration)("T.T[] y;");
    testFormatNode!(VariableDeclaration)("T.T[8] y;");
    testFormatNode!(VariableDeclaration)("T.T[8].T y;");
    testFormatNode!(VariableDeclaration)(`.T!"af" taf;`);
    testFormatNode!(VariableDeclaration)(`.T!0[] t;`);
    testFormatNode!(VariableDeclaration)(`T!(0)[] t;`);
    testFormatNode!(VariableDeclaration)(`T!(0)[dim] t;`);
    testFormatNode!(VariableDeclaration)(`const shared t = [0, 1];`);

    testFormatNode!(OutStatement)(
`
void foo()
out{static if (true) {baz();}}
do{}
`,
`out
{
    static if (true)
    {
        baz();
    }
}`);

    testFormatNode!(AutoDeclaration)("auto f = a;");
    testFormatNode!(AutoDeclaration)("auto f = ref () => a;");
    testFormatNode!(AliasDeclaration)("alias f = ref () => a;");

    testFormatNode!(AliasDeclaration)("alias int F(int);");
    testFormatNode!(AliasDeclaration)("alias const int F(int);");
    testFormatNode!(AliasDeclaration)("alias const int F(int) nothrow;");
    testFormatNode!(AliasDeclaration)("alias f(T) = void(T t);");
    testFormatNode!(AliasDeclaration)("alias f(T) = void(int, int, int) const pure nothrow;");

    testFormatNode!(AliasDeclaration)("alias MT = mixin (strEnum1, strEnum2);");

    testFormatNode!(Module)("static assert(() @trusted { return bar()(3); }() == 4);",
`static assert (() @trusted
{ return bar()(3);
}() == 4)`
);
    testFormatNode!(Module)("int foo() { return 2; }",
`int foo()
{
    return 2;
}`
);

    testFormatNode!(Module)("int foo() do { return 2; }",
`int foo()
do
{
    return 2;
}`
);

    testFormatNode!(Module)("int foo(int i) in(i > 0) do { return 2; }",
`int foo(int i)
in (i > 0)
do
{
    return 2;
}`
);

    testFormatNode!(Module)("int foo(int i) in { assert(i > 0); } do { return 2; }",
`int foo(int i)
in
{
    assert (i > 0);
}
do
{
    return 2;
}`
);

    testFormatNode!(Module)("int foo() out(j; j > 0) do { return 2; }",
`int foo()
out (j; j > 0)
do
{
    return 2;
}`
);

    testFormatNode!(Module)("int foo() out(j) { assert(j > 0); } do { return 2; }",
`int foo()
out (j)
{
    assert (j > 0);
}
do
{
    return 2;
}`
);

    testFormatNode!(Module)("version(BAR) {
        int foo(int i)
        in(i > 0) in { assert(i > 0); }
        out(j; j > 0) out(j) { assert(j>0); }
        do { return 2; } int i; }
    ",
`version (BAR)
{

    int foo(int i)
    in (i > 0)
    in
    {
        assert (i > 0);
    }
    out (j; j > 0)
    out (j)
    {
        assert (j > 0);
    }
    do
    {
        return 2;
    }

    int i;
}`
);
    testFormatNode!(Declaration)(q{int i = throw new Ex();});
    testFormatNode!(FunctionDeclaration)(q{void someFunction()
{
    foo(a, throw b, c);
    return throw new Exception("", "");
}});
}
