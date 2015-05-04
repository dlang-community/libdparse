// Written in the D programming language.

/**
 * This module defines an Abstract Syntax Tree for the D language
 *
 * Examples:
 * ---
 * // TODO
 * ---
 *
 * Copyright: Brian Schott 2013
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt Boost, License 1.0)
 * Authors: Brian Schott
 * Source: $(PHOBOSSRC std/d/_ast.d)
 */

module std.d.ast;

import std.d.lexer;
import std.traits;
import std.algorithm;
import std.array;
import std.string;

// TODO: Many of these classes can be simplified by using std.variant.Algebraic

/**
 * Implements the $(LINK2 http://en.wikipedia.org/wiki/Visitor_pattern, Visitor Pattern)
 * for the various AST classes
 */
abstract class ASTVisitor
{
public:

    void visit(const ExpressionNode n)
    {
        if (cast(AddExpression) n) visit(cast(AddExpression) n);
        else if (cast(AndAndExpression) n) visit(cast(AndAndExpression) n);
        else if (cast(AndExpression) n) visit(cast(AndExpression) n);
        else if (cast(AsmAddExp) n) visit(cast(AsmAddExp) n);
        else if (cast(AsmAndExp) n) visit(cast(AsmAndExp) n);
        else if (cast(AsmBrExp) n) visit(cast(AsmBrExp) n);
        else if (cast(AsmExp) n) visit(cast(AsmExp) n);
        else if (cast(AsmEqualExp) n) visit(cast(AsmEqualExp) n);
        else if (cast(AsmLogAndExp) n) visit(cast(AsmLogAndExp) n);
        else if (cast(AsmLogOrExp) n) visit(cast(AsmLogOrExp) n);
        else if (cast(AsmMulExp) n) visit(cast(AsmMulExp) n);
        else if (cast(AsmOrExp) n) visit(cast(AsmOrExp) n);
        else if (cast(AsmRelExp) n) visit(cast(AsmRelExp) n);
        else if (cast(AsmUnaExp) n) visit(cast(AsmUnaExp) n);
        else if (cast(AsmShiftExp) n) visit(cast(AsmShiftExp) n);
        else if (cast(AsmXorExp) n) visit(cast(AsmXorExp) n);
        else if (cast(AssertExpression) n) visit(cast(AssertExpression) n);
        else if (cast(AssignExpression) n) visit(cast(AssignExpression) n);
        else if (cast(CmpExpression) n) visit(cast(CmpExpression) n);
        else if (cast(DeleteExpression) n) visit(cast(DeleteExpression) n);
        else if (cast(EqualExpression) n) visit(cast(EqualExpression) n);
        else if (cast(Expression) n) visit(cast(Expression) n);
        else if (cast(FunctionCallExpression) n) visit(cast(FunctionCallExpression) n);
        else if (cast(FunctionLiteralExpression) n) visit(cast(FunctionLiteralExpression) n);
        else if (cast(IdentityExpression) n) visit(cast(IdentityExpression) n);
        else if (cast(ImportExpression) n) visit(cast(ImportExpression) n);
        else if (cast(IndexExpression) n) visit(cast(IndexExpression) n);
        else if (cast(InExpression) n) visit(cast(InExpression) n);
        else if (cast(IsExpression) n) visit(cast(IsExpression) n);
        else if (cast(LambdaExpression) n) visit(cast(LambdaExpression) n);
        else if (cast(MixinExpression) n) visit(cast(MixinExpression) n);
        else if (cast(MulExpression) n) visit(cast(MulExpression) n);
        else if (cast(NewAnonClassExpression) n) visit(cast(NewAnonClassExpression) n);
        else if (cast(NewExpression) n) visit(cast(NewExpression) n);
        else if (cast(OrExpression) n) visit(cast(OrExpression) n);
        else if (cast(OrOrExpression) n) visit(cast(OrOrExpression) n);
        else if (cast(PowExpression) n) visit(cast(PowExpression) n);
        else if (cast(PragmaExpression) n) visit(cast(PragmaExpression) n);
        else if (cast(PrimaryExpression) n) visit(cast(PrimaryExpression) n);
        else if (cast(RelExpression) n) visit(cast(RelExpression) n);
        else if (cast(ShiftExpression) n) visit(cast(ShiftExpression) n);
        else if (cast(SliceExpression) n) visit(cast(SliceExpression) n);
        else if (cast(TemplateMixinExpression) n) visit(cast(TemplateMixinExpression) n);
        else if (cast(TernaryExpression) n) visit(cast(TernaryExpression) n);
        else if (cast(TraitsExpression) n) visit(cast(TraitsExpression) n);
        else if (cast(TypeidExpression) n) visit(cast(TypeidExpression) n);
        else if (cast(TypeofExpression) n) visit(cast(TypeofExpression) n);
        else if (cast(UnaryExpression) n) visit(cast(UnaryExpression) n);
        else if (cast(XorExpression) n) visit(cast(XorExpression) n);
    }

    /** */ void visit(const AddExpression addExpression) { addExpression.accept(this); }
    /** */ void visit(const AliasDeclaration aliasDeclaration) { aliasDeclaration.accept(this); }
    /** */ void visit(const AliasInitializer aliasInitializer) { aliasInitializer.accept(this); }
    /** */ void visit(const AliasThisDeclaration aliasThisDeclaration) { aliasThisDeclaration.accept(this); }
    /** */ void visit(const AlignAttribute alignAttribute) { alignAttribute.accept(this); }
    /** */ void visit(const AndAndExpression andAndExpression) { andAndExpression.accept(this); }
    /** */ void visit(const AndExpression andExpression) { andExpression.accept(this); }
    /** */ void visit(const AnonymousEnumDeclaration anonymousEnumDeclaration) { anonymousEnumDeclaration.accept(this); }
    /** */ void visit(const AnonymousEnumMember anonymousEnumMember) { anonymousEnumMember.accept(this); }
    /** */ void visit(const ArgumentList argumentList) { argumentList.accept(this); }
    /** */ void visit(const Arguments arguments) { arguments.accept(this); }
    /** */ void visit(const ArrayInitializer arrayInitializer) { arrayInitializer.accept(this); }
    /** */ void visit(const ArrayLiteral arrayLiteral) { arrayLiteral.accept(this); }
    /** */ void visit(const ArrayMemberInitialization arrayMemberInitialization) { arrayMemberInitialization.accept(this); }
    /** */ void visit(const AsmAddExp asmAddExp) { asmAddExp.accept(this); }
    /** */ void visit(const AsmAndExp asmAndExp) { asmAndExp.accept(this); }
    /** */ void visit(const AsmBrExp asmBrExp) { asmBrExp.accept(this); }
    /** */ void visit(const AsmEqualExp asmEqualExp) { asmEqualExp.accept(this); }
    /** */ void visit(const AsmExp asmExp) { asmExp.accept(this); }
    /** */ void visit(const AsmInstruction asmInstruction) { asmInstruction.accept(this); }
    /** */ void visit(const AsmLogAndExp asmLogAndExp) { asmLogAndExp.accept(this); }
    /** */ void visit(const AsmLogOrExp asmLogOrExp) { asmLogOrExp.accept(this); }
    /** */ void visit(const AsmMulExp asmMulExp) { asmMulExp.accept(this); }
    /** */ void visit(const AsmOrExp asmOrExp) { asmOrExp.accept(this); }
    /** */ void visit(const AsmPrimaryExp asmPrimaryExp) { asmPrimaryExp.accept(this); }
    /** */ void visit(const AsmRelExp asmRelExp) { asmRelExp.accept(this); }
    /** */ void visit(const AsmShiftExp asmShiftExp) { asmShiftExp.accept(this); }
    /** */ void visit(const AsmStatement asmStatement) { asmStatement.accept(this); }
    /** */ void visit(const AsmTypePrefix asmTypePrefix) { asmTypePrefix.accept(this); }
    /** */ void visit(const AsmUnaExp asmUnaExp) { asmUnaExp.accept(this); }
    /** */ void visit(const AsmXorExp asmXorExp) { asmXorExp.accept(this); }
    /** */ void visit(const AssertExpression assertExpression) { assertExpression.accept(this); }
    /** */ void visit(const AssignExpression assignExpression) { assignExpression.accept(this); }
    /** */ void visit(const AssocArrayLiteral assocArrayLiteral) { assocArrayLiteral.accept(this); }
    /** */ void visit(const AtAttribute atAttribute) { atAttribute.accept(this); }
    /** */ void visit(const Attribute attribute) { attribute.accept(this); }
    /** */ void visit(const AttributeDeclaration attributeDeclaration) { attributeDeclaration.accept(this); }
    /** */ void visit(const AutoDeclaration autoDeclaration) { autoDeclaration.accept(this); }
    /** */ void visit(const BlockStatement blockStatement) { blockStatement.accept(this); }
    /** */ void visit(const BodyStatement bodyStatement) { bodyStatement.accept(this); }
    /** */ void visit(const BreakStatement breakStatement) { breakStatement.accept(this); }
    /** */ void visit(const BaseClass baseClass) { baseClass.accept(this); }
    /** */ void visit(const BaseClassList baseClassList) { baseClassList.accept(this); }
    /** */ void visit(const CaseRangeStatement caseRangeStatement) { caseRangeStatement.accept(this); }
    /** */ void visit(const CaseStatement caseStatement) { caseStatement.accept(this); }
    /** */ void visit(const CastExpression castExpression) { castExpression.accept(this); }
    /** */ void visit(const CastQualifier castQualifier) { castQualifier.accept(this); }
    /** */ void visit(const Catch catch_) { catch_.accept(this); }
    /** */ void visit(const Catches catches) { catches.accept(this); }
    /** */ void visit(const ClassDeclaration classDeclaration) { classDeclaration.accept(this); }
    /** */ void visit(const CmpExpression cmpExpression) { cmpExpression.accept(this); }
    /** */ void visit(const CompileCondition compileCondition) { compileCondition.accept(this); }
    /** */ void visit(const ConditionalDeclaration conditionalDeclaration) { conditionalDeclaration.accept(this); }
    /** */ void visit(const ConditionalStatement conditionalStatement) { conditionalStatement.accept(this); }
    /** */ void visit(const Constraint constraint) { constraint.accept(this); }
    /** */ void visit(const Constructor constructor) { constructor.accept(this); }
    /** */ void visit(const ContinueStatement continueStatement) { continueStatement.accept(this); }
    /** */ void visit(const DebugCondition debugCondition) { debugCondition.accept(this); }
    /** */ void visit(const DebugSpecification debugSpecification) { debugSpecification.accept(this); }
    /** */ void visit(const Declaration declaration) { declaration.accept(this); }
    /** */ void visit(const DeclarationOrStatement declarationsOrStatement) { declarationsOrStatement.accept(this); }
    /** */ void visit(const DeclarationsAndStatements declarationsAndStatements) { declarationsAndStatements.accept(this); }
    /** */ void visit(const Declarator declarator) { declarator.accept(this); }
    /** */ void visit(const DefaultStatement defaultStatement) { defaultStatement.accept(this); }
    /** */ void visit(const DeleteExpression deleteExpression) { deleteExpression.accept(this); }
    /** */ void visit(const DeleteStatement deleteStatement) { deleteStatement.accept(this); }
    /** */ void visit(const Deprecated deprecated_) { deprecated_.accept(this); }
    /** */ void visit(const Destructor destructor) { destructor.accept(this); }
    /** */ void visit(const DoStatement doStatement) { doStatement.accept(this); }
    /** */ void visit(const EnumBody enumBody) { enumBody.accept(this); }
    /** */ void visit(const EnumDeclaration enumDeclaration) { enumDeclaration.accept(this); }
    /** */ void visit(const EnumMember enumMember) { enumMember.accept(this); }
    /** */ void visit(const EponymousTemplateDeclaration eponymousTemplateDeclaration) { eponymousTemplateDeclaration.accept(this); }
    /** */ void visit(const EqualExpression equalExpression) { equalExpression.accept(this); }
    /** */ void visit(const Expression expression) { expression.accept(this); }
    /** */ void visit(const ExpressionStatement expressionStatement) { expressionStatement.accept(this); }
    /** */ void visit(const FinalSwitchStatement finalSwitchStatement) { finalSwitchStatement.accept(this); }
    /** */ void visit(const Finally finally_) { finally_.accept(this); }
    /** */ void visit(const ForStatement forStatement) { forStatement.accept(this); }
    /** */ void visit(const ForeachStatement foreachStatement) { foreachStatement.accept(this); }
    /** */ void visit(const ForeachType foreachType) { foreachType.accept(this); }
    /** */ void visit(const ForeachTypeList foreachTypeList) { foreachTypeList.accept(this); }
    /** */ void visit(const FunctionAttribute functionAttribute) { functionAttribute.accept(this); }
    /** */ void visit(const FunctionBody functionBody) { functionBody.accept(this); }
    /** */ void visit(const FunctionCallExpression functionCallExpression) { functionCallExpression.accept(this); }
    /** */ void visit(const FunctionDeclaration functionDeclaration) { functionDeclaration.accept(this); }
    /** */ void visit(const FunctionLiteralExpression functionLiteralExpression) { functionLiteralExpression.accept(this); }
    /** */ void visit(const GotoStatement gotoStatement) { gotoStatement.accept(this); }
    /** */ void visit(const IdentifierChain identifierChain) { identifierChain.accept(this); }
    /** */ void visit(const IdentifierList identifierList) { identifierList.accept(this); }
    /** */ void visit(const IdentifierOrTemplateChain identifierOrTemplateChain) { identifierOrTemplateChain.accept(this); }
    /** */ void visit(const IdentifierOrTemplateInstance identifierOrTemplateInstance) { identifierOrTemplateInstance.accept(this); }
    /** */ void visit(const IdentityExpression identityExpression) { identityExpression.accept(this); }
    /** */ void visit(const IfStatement ifStatement) { ifStatement.accept(this); }
    /** */ void visit(const ImportBind importBind) { importBind.accept(this); }
    /** */ void visit(const ImportBindings importBindings) { importBindings.accept(this); }
    /** */ void visit(const ImportDeclaration importDeclaration) { importDeclaration.accept(this); }
    /** */ void visit(const ImportExpression importExpression) { importExpression.accept(this); }
    /** */ void visit(const IndexExpression indexExpression) { indexExpression.accept(this); }
    /** */ void visit(const InExpression inExpression) { inExpression.accept(this); }
    /** */ void visit(const InStatement inStatement) { inStatement.accept(this); }
    /** */ void visit(const Initialize initialize) { initialize.accept(this); }
    /** */ void visit(const Initializer initializer) { initializer.accept(this); }
    /** */ void visit(const InterfaceDeclaration interfaceDeclaration) { interfaceDeclaration.accept(this); }
    /** */ void visit(const Invariant invariant_) { invariant_.accept(this); }
    /** */ void visit(const IsExpression isExpression) { isExpression.accept(this); }
    /** */ void visit(const KeyValuePair keyValuePair) { keyValuePair.accept(this); }
    /** */ void visit(const KeyValuePairs keyValuePairs) { keyValuePairs.accept(this); }
    /** */ void visit(const LabeledStatement labeledStatement) { labeledStatement.accept(this); }
    /** */ void visit(const LambdaExpression lambdaExpression) { lambdaExpression.accept(this); }
    /** */ void visit(const LastCatch lastCatch) { lastCatch.accept(this); }
    /** */ void visit(const LinkageAttribute linkageAttribute) { linkageAttribute.accept(this); }
    /** */ void visit(const MemberFunctionAttribute memberFunctionAttribute) { memberFunctionAttribute.accept(this); }
    /** */ void visit(const MixinDeclaration mixinDeclaration) { mixinDeclaration.accept(this); }
    /** */ void visit(const MixinExpression mixinExpression) { mixinExpression.accept(this); }
    /** */ void visit(const MixinTemplateDeclaration mixinTemplateDeclaration) { mixinTemplateDeclaration.accept(this); }
    /** */ void visit(const MixinTemplateName mixinTemplateName) { mixinTemplateName.accept(this); }
    /** */ void visit(const Module module_) { module_.accept(this); }
    /** */ void visit(const ModuleDeclaration moduleDeclaration) { moduleDeclaration.accept(this); }
    /** */ void visit(const MulExpression mulExpression) { mulExpression.accept(this); }
    /** */ void visit(const NewAnonClassExpression newAnonClassExpression) { newAnonClassExpression.accept(this); }
    /** */ void visit(const NewExpression newExpression) { newExpression.accept(this); }
    /** */ void visit(const NonVoidInitializer nonVoidInitializer) { nonVoidInitializer.accept(this); }
    /** */ void visit(const Operands operands) { operands.accept(this); }
    /** */ void visit(const OrExpression orExpression) { orExpression.accept(this); }
    /** */ void visit(const OrOrExpression orOrExpression) { orOrExpression.accept(this); }
    /** */ void visit(const OutStatement outStatement) { outStatement.accept(this); }
    /** */ void visit(const Parameter parameter) { parameter.accept(this); }
    /** */ void visit(const Parameters parameters) { parameters.accept(this); }
    /** */ void visit(const Postblit postblit) { postblit.accept(this); }
    /** */ void visit(const PowExpression powExpression) { powExpression.accept(this); }
    /** */ void visit(const PragmaDeclaration pragmaDeclaration) { pragmaDeclaration.accept(this); }
    /** */ void visit(const PragmaExpression pragmaExpression) { pragmaExpression.accept(this); }
    /** */ void visit(const PrimaryExpression primaryExpression) { primaryExpression.accept(this); }
    /** */ void visit(const Register register) { register.accept(this); }
    /** */ void visit(const RelExpression relExpression) { relExpression.accept(this); }
    /** */ void visit(const ReturnStatement returnStatement) { returnStatement.accept(this); }
    /** */ void visit(const ScopeGuardStatement scopeGuardStatement) { scopeGuardStatement.accept(this); }
    /** */ void visit(const SharedStaticConstructor sharedStaticConstructor) { sharedStaticConstructor.accept(this); }
    /** */ void visit(const SharedStaticDestructor sharedStaticDestructor) { sharedStaticDestructor.accept(this); }
    /** */ void visit(const ShiftExpression shiftExpression) { shiftExpression.accept(this); }
    /** */ void visit(const SingleImport singleImport) { singleImport.accept(this); }
    /** */ void visit(const SliceExpression sliceExpression) { sliceExpression.accept(this); }
    /** */ void visit(const Statement statement) { statement.accept(this); }
    /** */ void visit(const StatementNoCaseNoDefault statementNoCaseNoDefault) { statementNoCaseNoDefault.accept(this); }
    /** */ void visit(const StaticAssertDeclaration staticAssertDeclaration) { staticAssertDeclaration.accept(this); }
    /** */ void visit(const StaticAssertStatement staticAssertStatement) { staticAssertStatement.accept(this); }
    /** */ void visit(const StaticConstructor staticConstructor) { staticConstructor.accept(this); }
    /** */ void visit(const StaticDestructor staticDestructor) { staticDestructor.accept(this); }
    /** */ void visit(const StaticIfCondition staticIfCondition) { staticIfCondition.accept(this); }
    /** */ void visit(const StorageClass storageClass) { storageClass.accept(this); }
    /** */ void visit(const StructBody structBody) { structBody.accept(this); }
    /** */ void visit(const StructDeclaration structDeclaration) { structDeclaration.accept(this); }
    /** */ void visit(const StructInitializer structInitializer) { structInitializer.accept(this); }
    /** */ void visit(const StructMemberInitializer structMemberInitializer) { structMemberInitializer.accept(this); }
    /** */ void visit(const StructMemberInitializers structMemberInitializers) { structMemberInitializers.accept(this); }
    /** */ void visit(const SwitchStatement switchStatement) { switchStatement.accept(this); }
    /** */ void visit(const Symbol symbol) { symbol.accept(this); }
    /** */ void visit(const SynchronizedStatement synchronizedStatement) { synchronizedStatement.accept(this); }
    /** */ void visit(const TemplateAliasParameter templateAliasParameter) { templateAliasParameter.accept(this); }
    /** */ void visit(const TemplateArgument templateArgument) { templateArgument.accept(this); }
    /** */ void visit(const TemplateArgumentList templateArgumentList) { templateArgumentList.accept(this); }
    /** */ void visit(const TemplateArguments templateArguments) { templateArguments.accept(this); }
    /** */ void visit(const TemplateDeclaration templateDeclaration) { templateDeclaration.accept(this); }
    /** */ void visit(const TemplateInstance templateInstance) { templateInstance.accept(this); }
    /** */ void visit(const TemplateMixinExpression templateMixinExpression) { templateMixinExpression.accept(this); }
    /** */ void visit(const TemplateParameter templateParameter) { templateParameter.accept(this); }
    /** */ void visit(const TemplateParameterList templateParameterList) { templateParameterList.accept(this); }
    /** */ void visit(const TemplateParameters templateParameters) { templateParameters.accept(this); }
    /** */ void visit(const TemplateSingleArgument templateSingleArgument) { templateSingleArgument.accept(this); }
    /** */ void visit(const TemplateThisParameter templateThisParameter) { templateThisParameter.accept(this); }
    /** */ void visit(const TemplateTupleParameter templateTupleParameter) { templateTupleParameter.accept(this); }
    /** */ void visit(const TemplateTypeParameter templateTypeParameter) { templateTypeParameter.accept(this); }
    /** */ void visit(const TemplateValueParameter templateValueParameter) { templateValueParameter.accept(this); }
    /** */ void visit(const TemplateValueParameterDefault templateValueParameterDefault) { templateValueParameterDefault.accept(this); }
    /** */ void visit(const TernaryExpression ternaryExpression) { ternaryExpression.accept(this); }
    /** */ void visit(const ThrowStatement throwStatement) { throwStatement.accept(this); }
    /** */ void visit(const Token) { }
    /** */ void visit(const TraitsExpression traitsExpression) { traitsExpression.accept(this); }
    /** */ void visit(const TryStatement tryStatement) { tryStatement.accept(this); }
    /** */ void visit(const Type type) { type.accept(this); }
    /** */ void visit(const Type2 type2) { type2.accept(this); }
    /** */ void visit(const TypeSpecialization typeSpecialization) { typeSpecialization.accept(this); }
    /** */ void visit(const TypeSuffix typeSuffix) { typeSuffix.accept(this); }
    /** */ void visit(const TypeidExpression typeidExpression) { typeidExpression.accept(this); }
    /** */ void visit(const TypeofExpression typeofExpression) { typeofExpression.accept(this); }
    /** */ void visit(const UnaryExpression unaryExpression) { unaryExpression.accept(this); }
    /** */ void visit(const UnionDeclaration unionDeclaration) { unionDeclaration.accept(this); }
    /** */ void visit(const Unittest unittest_) { unittest_.accept(this); }
    /** */ void visit(const VariableDeclaration variableDeclaration) { variableDeclaration.accept(this); }
    /** */ void visit(const Vector vector) { vector.accept(this); }
    /** */ void visit(const VersionCondition versionCondition) { versionCondition.accept(this); }
    /** */ void visit(const VersionSpecification versionSpecification) { versionSpecification.accept(this); }
    /** */ void visit(const WhileStatement whileStatement) { whileStatement.accept(this); }
    /** */ void visit(const WithStatement withStatement) { withStatement.accept(this); }
    /** */ void visit(const XorExpression xorExpression) { xorExpression.accept(this); }
}

interface ASTNode
{
public:
    /** */ void accept(ASTVisitor visitor) const;
}

template visitIfNotNull(fields ...)
{
    static if (fields.length > 1)
        immutable visitIfNotNull = visitIfNotNull!(fields[0]) ~ visitIfNotNull!(fields[1..$]);
    else
    {
        static if (typeof(fields[0]).stringof[$ - 2 .. $] == "[]")
        {
            static if (__traits(hasMember, typeof(fields[0][0]), "classinfo"))
                immutable visitIfNotNull = "foreach (i; " ~ fields[0].stringof ~ ") if (i !is null) visitor.visit(i);\n";
            else
                immutable visitIfNotNull = "foreach (i; " ~ fields[0].stringof ~ ") visitor.visit(i);\n";
        }
        else static if (__traits(hasMember, typeof(fields[0]), "classinfo"))
            immutable visitIfNotNull = "if (" ~ fields[0].stringof ~ " !is null) visitor.visit(" ~ fields[0].stringof ~ ");\n";
        else
            immutable visitIfNotNull = "visitor.visit(" ~ fields[0].stringof ~ ");\n";
    }
}

mixin template OpEquals(bool print = false)
{
    override bool opEquals(Object other) const
    {
        static if (print)
            pragma(msg, generateOpEquals!(typeof(this)));
        mixin (generateOpEquals!(typeof(this)));
    }
}

template generateOpEquals(T)
{
    template opEqualsPart(p ...)
    {
        import std.traits;
        static if (p.length == 0)
            enum opEqualsPart = "";
        else static if (!isSomeFunction!(__traits(getMember, T, p[0]))
            && p[0] != "line" && p[0] != "column" && p[0] != "startLocation"
            && p[0] != "endLocation" && p[0] != "index")
        {
            static if (typeof(__traits(getMember, T, p[0])).stringof[$ - 2 .. $] == "[]")
            {
                enum opEqualsPart = "\nif (obj." ~ p[0] ~ ".length != this." ~ p[0] ~ ".length) return false;\n"
                    ~ "foreach (i; 0 .. this." ~ p[0] ~ ".length)\n"
                    ~ "\tif (this." ~ p[0] ~ "[i] != obj." ~ p[0] ~ "[i]) return false;" ~ opEqualsPart!(p[1 .. $]);
            }
            else
                enum opEqualsPart = "\nif (obj." ~ p[0] ~ " != this." ~ p[0] ~ ") return false;" ~ opEqualsPart!(p[1 .. $]);
        }
        else
            enum opEqualsPart = opEqualsPart!(p[1 .. $]);
    }
    enum generateOpEquals = T.stringof ~ " obj = cast(" ~ T.stringof ~ ") other;\n"
        ~ "if (obj is null) return false;"
        ~ opEqualsPart!(__traits(derivedMembers, T)) ~ "\n"
        ~ "return true;";
}

abstract class ExpressionNode : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        assert (false);
    }
}

mixin template BinaryExpressionBody()
{
    ExpressionNode left;
    ExpressionNode right;
    size_t line;
    size_t column;
}

///
final class AddExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin OpEquals;
    /** */ IdType operator;
    mixin BinaryExpressionBody;
}

///
final class AliasDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(storageClasses, type, identifierList, initializers));
    }
    mixin OpEquals;
    /** */ StorageClass[] storageClasses;
    /** */ Type type;
    /** */ IdentifierList identifierList;
    /** */ AliasInitializer[] initializers;
    /** */ string comment;
}

///
final class AliasInitializer : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(name, templateParameters, storageClasses, type));
    }
    mixin OpEquals;
    /** */ Token name;
    /** */ StorageClass[] storageClasses;
    /** */ TemplateParameters templateParameters;
    /** */ Type type;
}

///
final class AliasThisDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifier));
    }
    mixin OpEquals;
    /** */ Token identifier;
}

///
final class AlignAttribute : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(intLiteral));
    }
    mixin OpEquals;
    /** */ Token intLiteral;
}

///
final class AndAndExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin OpEquals;
    mixin BinaryExpressionBody;
}

///
final class AndExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin OpEquals;
    mixin BinaryExpressionBody;
}

///
final class AnonymousEnumDeclaration : ASTNode
{
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(baseType, members));
    }
    mixin OpEquals;
    /** */ Type baseType;
    /** */ AnonymousEnumMember[] members;
}

///
final class AnonymousEnumMember : ASTNode
{
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type, name, assignExpression));
    }
    /** */ Type type;
    /** */ Token name;
    /** */ ExpressionNode assignExpression;
    /** */ string comment;
}

///
final class ArgumentList : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(items));
    }
    mixin OpEquals;
    /** */ ExpressionNode[] items;
    /** */ size_t startLocation;
    /** */ size_t endLocation;
}

///
final class Arguments : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(argumentList));
    }
    mixin OpEquals;
    /** */ ArgumentList argumentList;
}

///
final class ArrayInitializer : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(arrayMemberInitializations));
    }
    mixin OpEquals;
    /** */ size_t startLocation;
    /** */ size_t endLocation;
    /** */ ArrayMemberInitialization[] arrayMemberInitializations;
}

///
final class ArrayLiteral : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(argumentList));
    }
    mixin OpEquals;
    /** */ ArgumentList argumentList;
}

///
final class ArrayMemberInitialization : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(assignExpression, nonVoidInitializer));
    }
    mixin OpEquals;
    /** */ ExpressionNode assignExpression;
    /** */ NonVoidInitializer nonVoidInitializer;
}

///
final class AsmAddExp : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin OpEquals;
    /** */ IdType operator;
    mixin BinaryExpressionBody;
}

///
final class AsmAndExp : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin OpEquals;
    mixin BinaryExpressionBody;
}

///
final class AsmBrExp : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(asmBrExp, asmExp, asmUnaExp));
    }
    mixin OpEquals;
    size_t line;
    size_t column;
    /** */ AsmBrExp asmBrExp;
    /** */ ExpressionNode asmExp;
    /** */ AsmUnaExp asmUnaExp;
}

///
final class AsmEqualExp : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
    mixin OpEquals;
    /** */ IdType operator;
}

///
final class AsmExp : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, middle, right));
    }
    mixin OpEquals;
    /** */ ExpressionNode left;
    /** */ ExpressionNode middle;
    /** */ ExpressionNode right;
}

///
final class AsmInstruction : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifierOrIntegerOrOpcode, asmInstruction, operands));
    }
    mixin OpEquals;
    /** */ Token identifierOrIntegerOrOpcode;
    /** */ bool hasAlign;
    /** */ AsmInstruction asmInstruction;
    /** */ Operands operands;
}

///
final class AsmLogAndExp : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class AsmLogOrExp : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class AsmMulExp : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    /** */ IdType operator;
    mixin BinaryExpressionBody;
    mixin OpEquals;

}

///
final class AsmOrExp : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class AsmPrimaryExp : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(token, register, asmExp, identifierChain));
    }
    /** */ ExpressionNode asmExp;
    /** */ IdentifierChain identifierChain;
    /** */ Register register;
    /** */ Token token;
    mixin OpEquals;
}

///
final class AsmRelExp : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
    /** */ IdType operator;
    mixin OpEquals;
}

///
final class AsmShiftExp : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
    /** */ IdType operator;
    mixin OpEquals;
}

///
final class AsmStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!asmInstructions);
    }
    /** */ AsmInstruction[] asmInstructions;
    /** */ FunctionAttribute[] functionAttributes;
    mixin OpEquals;
}

///
final class AsmTypePrefix : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    /** */ Token left;
    /** */ Token right;
    mixin OpEquals;
}

///
final class AsmUnaExp : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(prefix, asmTypePrefix, asmExp, asmPrimaryExp, asmUnaExp));
    }
    /** */ AsmTypePrefix asmTypePrefix;
    /** */ ExpressionNode asmExp;
    /** */ Token prefix;
    /** */ AsmPrimaryExp asmPrimaryExp;
    /** */ AsmUnaExp asmUnaExp;
    mixin OpEquals;
}

///
final class AsmXorExp : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class AssertExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(assertion, message));
    }
    /** */ ExpressionNode assertion;
    /** */ ExpressionNode message;
    mixin OpEquals;
}

///
final class AssignExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(ternaryExpression, assignExpression));
    }
    /** */ ExpressionNode ternaryExpression;
    /** */ ExpressionNode assignExpression;
    /** */ IdType operator;
    /** */ size_t line;
    /** */ size_t column;
    mixin OpEquals;
}

///
final class AssocArrayLiteral : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(keyValuePairs));
    }
    /** */ KeyValuePairs keyValuePairs;
    mixin OpEquals;
}

///
final class AtAttribute : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(argumentList));
    }
    /** */ ArgumentList argumentList;
    /** */ Token identifier;
    /** */ size_t startLocation;
    /** */ size_t endLocation;
    mixin OpEquals;
}

///
final class Attribute : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(pragmaExpression, deprecated_, atAttribute,
            alignAttribute, identifierChain));
    }
    /** */ PragmaExpression pragmaExpression;
    /** */ Deprecated deprecated_;
    /** */ AtAttribute atAttribute;
    /** */ AlignAttribute alignAttribute;
    /** */ LinkageAttribute linkageAttribute;
    /** */ Token attribute;
    /** */ IdentifierChain identifierChain;
    mixin OpEquals;
}

///
final class AttributeDeclaration : ASTNode
{
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(attribute));
    }
    /** */ Attribute attribute;
    /** */ size_t line;
    mixin OpEquals;
}

///
final class AutoDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        foreach (sc; storageClasses)
            visitor.visit(sc);
        foreach (i; 0 .. initializers.length)
        {
            visitor.visit(initializers[i]);
        }
    }
    /** */ Token[] identifiers;
    /** */ Initializer[] initializers;
    /** */ StorageClass[] storageClasses;
    /** */ string comment;
    mixin OpEquals;
}

///
final class BlockStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(declarationsAndStatements));
    }

    /**
     * Byte position of the opening brace
     */
    size_t startLocation;

    /**
     * Byte position of the closing brace
     */
    size_t endLocation;

    /** */ DeclarationsAndStatements declarationsAndStatements;
    mixin OpEquals;
}

///
final class BodyStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(blockStatement));
    }
    /** */ BlockStatement blockStatement;
    mixin OpEquals;
}

///
final class BreakStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(label));
    }
    /** */ Token label;
    mixin OpEquals;
}

///
final class BaseClass : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type2));
    }
    /** */ Type2 type2;
    mixin OpEquals;
}

///
final class BaseClassList : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(items));
    }
    /** */ BaseClass[] items;
    mixin OpEquals;
}

///
final class CaseRangeStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(low, high, declarationsAndStatements));
    }
    /** */ ExpressionNode low;
    /** */ ExpressionNode high;
    /** */ DeclarationsAndStatements declarationsAndStatements;
	/** */ size_t colonLocation;
    mixin OpEquals;
}

///
final class CaseStatement: ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(argumentList, declarationsAndStatements));
    }
    /** */ ArgumentList argumentList;
    /** */ DeclarationsAndStatements declarationsAndStatements;
	/** */ size_t colonLocation;
    mixin OpEquals;
}

///
final class CastExpression: ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type, castQualifier, unaryExpression));
    }
    /** */ Type type;
    /** */ CastQualifier castQualifier;
    /** */ UnaryExpression unaryExpression;
    mixin OpEquals;
}

///
final class CastQualifier: ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(first, second));
    }
    /** */ Token first;
    /** */ Token second;
    mixin OpEquals;
}

///
final class Catches: ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(catches, lastCatch));
    }
    /** */ Catch[] catches;
    /** */ LastCatch lastCatch;
    mixin OpEquals;
}

///
final class Catch: ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type, identifier, declarationOrStatement));
    }
    /** */ Type type;
    /** */ Token identifier;
    /** */ DeclarationOrStatement declarationOrStatement;
    mixin OpEquals;
}

///
final class ClassDeclaration: ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(templateParameters, constraint, baseClassList,
            structBody));
    }

    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ BaseClassList baseClassList;
    /** */ StructBody structBody;
    /** */ string comment;
    mixin OpEquals;
}

///
final class CmpExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(shiftExpression, equalExpression,
            identityExpression, relExpression, inExpression));
    }
    /** */ ExpressionNode shiftExpression;
    /** */ ExpressionNode equalExpression;
    /** */ ExpressionNode identityExpression;
    /** */ ExpressionNode relExpression;
    /** */ ExpressionNode inExpression;
    mixin OpEquals;
}

///
final class CompileCondition : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(versionCondition, debugCondition, staticIfCondition));
    }
    /** */ VersionCondition versionCondition;
    /** */ DebugCondition debugCondition;
    /** */ StaticIfCondition staticIfCondition;
    mixin OpEquals;
}

///
final class ConditionalDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(compileCondition, trueDeclarations, falseDeclaration));
    }
    /** */ CompileCondition compileCondition;
    /** */ Declaration[] trueDeclarations;
    /** */ Declaration falseDeclaration;
    mixin OpEquals;
}

///
final class ConditionalStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(compileCondition, trueStatement, falseStatement));
    }
    /** */ CompileCondition compileCondition;
    /** */ DeclarationOrStatement trueStatement;
    /** */ DeclarationOrStatement falseStatement;
    mixin OpEquals;
}

///
final class Constraint : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(expression));
    }
    /** */ Expression expression;
    mixin OpEquals;
}

///
final class Constructor : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(parameters, templateParameters, constraint,
            memberFunctionAttributes, functionBody));
    }
    /** */ Parameters parameters;
    /** */ FunctionBody functionBody;
    /** */ Constraint constraint;
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    /** */ TemplateParameters templateParameters;
    /** */ size_t location;
    /** */ size_t line;
    /** */ size_t column;
    /** */ string comment;
    mixin OpEquals;
}

///
final class ContinueStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(label));
    }
    /** */ Token label;
    mixin OpEquals;
}

///
final class DebugCondition : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifierOrInteger));
    }
    /** */ size_t debugIndex;
    /** */ Token identifierOrInteger;
    mixin OpEquals;
}

///
final class DebugSpecification : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifierOrInteger));
    }
    /** */ Token identifierOrInteger;
    mixin OpEquals;
}

///
final class Declaration : ASTNode
{
public:

    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(attributes, attributeDeclaration,
            importDeclaration, functionDeclaration,
            variableDeclaration, aliasThisDeclaration, structDeclaration,
            classDeclaration, interfaceDeclaration, unionDeclaration,
            enumDeclaration, aliasDeclaration, mixinDeclaration,
            mixinTemplateDeclaration, unittest_, staticAssertDeclaration,
            templateDeclaration, constructor,
            destructor, staticConstructor, staticDestructor,
            sharedStaticDestructor, sharedStaticConstructor,
            conditionalDeclaration, pragmaDeclaration, versionSpecification,
            invariant_, postblit, declarations, debugSpecification,
            eponymousTemplateDeclaration, anonymousEnumDeclaration));
    }

    /** */ AliasDeclaration aliasDeclaration;
    /** */ AliasThisDeclaration aliasThisDeclaration;
    /** */ AnonymousEnumDeclaration anonymousEnumDeclaration;
    /** */ Attribute[] attributes;
    /** */ AttributeDeclaration attributeDeclaration;
    /** */ ClassDeclaration classDeclaration;
    /** */ ConditionalDeclaration conditionalDeclaration;
    /** */ Constructor constructor;
    /** */ DebugSpecification debugSpecification;
    /** */ Declaration[] declarations;
    /** */ Destructor destructor;
    /** */ EnumDeclaration enumDeclaration;
    /** */ EponymousTemplateDeclaration eponymousTemplateDeclaration;
    /** */ FunctionDeclaration functionDeclaration;
    /** */ ImportDeclaration importDeclaration;
    /** */ InterfaceDeclaration interfaceDeclaration;
    /** */ Invariant invariant_;
    /** */ MixinDeclaration mixinDeclaration;
    /** */ MixinTemplateDeclaration mixinTemplateDeclaration;
    /** */ Postblit postblit;
    /** */ PragmaDeclaration pragmaDeclaration;
    /** */ SharedStaticConstructor sharedStaticConstructor;
    /** */ SharedStaticDestructor sharedStaticDestructor;
    /** */ StaticAssertDeclaration staticAssertDeclaration;
    /** */ StaticConstructor staticConstructor;
    /** */ StaticDestructor staticDestructor;
    /** */ StructDeclaration structDeclaration;
    /** */ TemplateDeclaration templateDeclaration;
    /** */ UnionDeclaration unionDeclaration;
    /** */ Unittest unittest_;
    /** */ VariableDeclaration variableDeclaration;
    /** */ VersionSpecification versionSpecification;
    mixin OpEquals;
}

///
final class DeclarationsAndStatements : ASTNode
{
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(declarationsAndStatements));
    }

    /** */ DeclarationOrStatement[] declarationsAndStatements;
    mixin OpEquals;
}

///
final class DeclarationOrStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(declaration, statement));
    }

    /** */ Declaration declaration;
    /** */ Statement statement;
    mixin OpEquals;
}

///
final class Declarator : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(templateParameters, initializer));
    }
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Initializer initializer;
    /** */ TypeSuffix[] cstyle;
    /** */ string comment;
    mixin OpEquals;
}

///
final class DefaultStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(declarationsAndStatements));
    }
    /** */ DeclarationsAndStatements declarationsAndStatements;
	/** */ size_t colonLocation;
    mixin OpEquals;
}

///
final class DeleteExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(unaryExpression));
    }
    /** */ UnaryExpression unaryExpression;
    /** */ size_t line;
    /** */ size_t column;
    mixin OpEquals;
}

///
final class DeleteStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(deleteExpression));
    }
    /** */ DeleteExpression deleteExpression;
    mixin OpEquals;
}

///
final class Deprecated : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(stringLiterals));
    }
    /** */ Token[] stringLiterals;
    mixin OpEquals;
}

///
final class Destructor : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(memberFunctionAttributes, functionBody));
    }
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    /** */ FunctionBody functionBody;
    /** */ size_t line;
    /** */ size_t column;
    /** */ size_t index;
    /** */ string comment;
    mixin OpEquals;
}

///
final class DoStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(expression, statementNoCaseNoDefault));
    }
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    /** */ Expression expression;
    mixin OpEquals;
}

///
final class EnumBody : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(enumMembers));
    }
    /** */ EnumMember[] enumMembers;

    /**
     * Byte position of the opening brace
     */
    size_t startLocation;

    /**
     * Byte position of the closing brace
     */
    size_t endLocation;
    mixin OpEquals;
}

///
final class EnumDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type, enumBody));
    }
    /** */ Token name;
    /** */ Type type;
    /** */ EnumBody enumBody;
    /** */ string comment;
    mixin OpEquals;
}

///
final class EnumMember : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(name, type, assignExpression));
    }
    /** */ Token name;
    /** */ Type type;
    /** */ ExpressionNode assignExpression;
    /** */ string comment;
    mixin OpEquals;
}

///
final class EponymousTemplateDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(name, templateParameters, assignExpression));
    }
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ ExpressionNode assignExpression;
    /** */ Type type;
    mixin OpEquals;
}

///
final class EqualExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    /** */ IdType operator;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class Expression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(items));
    }
    /** */ ExpressionNode[] items;
    /** */ size_t line;
    /** */ size_t column;
    mixin OpEquals;
}

///
final class ExpressionStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(expression));
    }
    /** */ Expression expression;
    mixin OpEquals;
}

///
final class FinalSwitchStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(switchStatement));
    }
    /** */ SwitchStatement switchStatement;
    mixin OpEquals;
}

///
final class Finally : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(declarationOrStatement));
    }
    /** */ DeclarationOrStatement declarationOrStatement;
    mixin OpEquals;
}

///
final class ForStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(initialization, test, increment,
            declarationOrStatement));
    }
    /** */ DeclarationOrStatement initialization;
    /** */ Expression test;
    /** */ Expression increment;
    /** */ DeclarationOrStatement declarationOrStatement;
    /** */ size_t startIndex;
    mixin OpEquals;
}

///
final class ForeachStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(foreachType, foreachTypeList, low, high,
            declarationOrStatement));
    }
    /** */ IdType type;
    /** */ ForeachTypeList foreachTypeList;
    /** */ ForeachType foreachType;
    /** */ Expression low;
    /** */ Expression high;
    /** */ DeclarationOrStatement declarationOrStatement;
    /** */ size_t startIndex;
    mixin OpEquals;
}

///
final class ForeachType : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type, identifier));
    }
    /** */ bool isRef;
    /** */ IdType[] typeConstructors;
    /** */ Type type;
    /** */ Token identifier;
    mixin OpEquals;
}

///
final class ForeachTypeList : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(items));
    }
    /** */ ForeachType[] items;
    mixin OpEquals;
}

///
final class FunctionAttribute : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(token, atAttribute));
    }
    /** */ Token token;
    /** */ AtAttribute atAttribute;
    mixin OpEquals;
}

///
final class FunctionBody : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(inStatement, outStatement, bodyStatement,
            blockStatement));
    }

    /** */ BlockStatement blockStatement;
    /** */ BodyStatement bodyStatement;
    /** */ OutStatement outStatement;
    /** */ InStatement inStatement;
    mixin OpEquals;
}

///
final class FunctionCallExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type, unaryExpression, templateArguments, arguments));
    }
    /** */ Type type;
    /** */ UnaryExpression unaryExpression;
    /** */ TemplateArguments templateArguments;
    /** */ Arguments arguments;
    mixin OpEquals;
}

///
final class FunctionDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(storageClasses, returnType, parameters,
            templateParameters, constraint, memberFunctionAttributes,
            functionBody));
    }
    /** */ bool hasAuto;
    /** */ bool hasRef;
    /** */ Type returnType;
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Parameters parameters;
    /** */ Constraint constraint;
    /** */ FunctionBody functionBody;
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    /** */ string comment;
    /** */ Attribute[] attributes;
    /** */ StorageClass[] storageClasses;
    mixin OpEquals;
}

///
final class FunctionLiteralExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type, parameters, functionAttributes,
            functionBody));
    }
    /** */ IdType functionOrDelegate;
    /** */ Type type;
    /** */ Parameters parameters;
    /** */ FunctionAttribute[] functionAttributes;
    /** */ FunctionBody functionBody;
    mixin OpEquals;
}

///
final class GotoStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(label, expression));
    }
    /** */ Expression expression;
    /** */ Token label;
    mixin OpEquals;
}

///
final class IdentifierChain : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifiers));
    }
    /** */ Token[] identifiers;
    mixin OpEquals;
}

///
final class IdentifierList : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifiers));
    }
    /** */ Token[] identifiers;
    mixin OpEquals;
}

///
final class IdentifierOrTemplateChain : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifiersOrTemplateInstances));
    }

    /** */ IdentifierOrTemplateInstance[] identifiersOrTemplateInstances;
    mixin OpEquals;
}

///
final class IdentifierOrTemplateInstance : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifier, templateInstance));
    }

    /** */ Token identifier;
    /** */ TemplateInstance templateInstance;
    mixin OpEquals;
}

///
final class IdentityExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    /** */ bool negated;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class IfStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifier, type, expression, thenStatement,
            elseStatement));
    }
    /** */ Token identifier;
    /** */ Type type;
    /** */ Expression expression;
    /** */ DeclarationOrStatement thenStatement;
    /** */ DeclarationOrStatement elseStatement;
    /** */ size_t startIndex;
    /** */ size_t line;
    /** */ size_t column;
    mixin OpEquals;
}

///
final class ImportBind : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    /** */ Token left;
    /** */ Token right;
    mixin OpEquals;
}

///
final class ImportBindings : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(singleImport, importBinds));
    }
    /** */ SingleImport singleImport;
    /** */ ImportBind[] importBinds;
    mixin OpEquals;
}

///
final class ImportDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(singleImports, importBindings));
    }
    /** */ SingleImport[] singleImports;
    /** */ ImportBindings importBindings;
    mixin OpEquals;
}

///
final class ImportExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(assignExpression));
    }
    /** */ ExpressionNode assignExpression;
    mixin OpEquals;
}

///
final class IndexExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(unaryExpression, argumentList));
    }
    /** */ UnaryExpression unaryExpression;
    /** */ ArgumentList argumentList;
    mixin OpEquals;
}

///
final class InExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
    bool negated;
    mixin OpEquals;
}

///
final class InStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(blockStatement));
    }
    /** */ size_t inTokenLocation;
    /** */ BlockStatement blockStatement;
    mixin OpEquals;
}

///
final class Initialize : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(statementNoCaseNoDefault));
    }
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    mixin OpEquals;
}

///
final class Initializer : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(nonVoidInitializer));
    }
    /** */ NonVoidInitializer nonVoidInitializer;
    mixin OpEquals;
}

///
final class InterfaceDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(templateParameters, constraint, baseClassList,
            structBody));
    }
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ BaseClassList baseClassList;
    /** */ StructBody structBody;
    /** */ string comment;
    mixin OpEquals;
}

///
final class Invariant : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(blockStatement));
    }
    /** */ BlockStatement blockStatement;
    /** */ string comment;
    size_t line;
    size_t index;
    mixin OpEquals;
}

///
final class IsExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type, identifier, typeSpecialization,
            templateParameterList));
    }
    /** */ Type type;
    /** */ Token identifier;
    /** */ TypeSpecialization typeSpecialization;
    /** */ TemplateParameterList templateParameterList;
    /** */ IdType equalsOrColon;
    mixin OpEquals;
}

///
final class KeyValuePair : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(key, value));
    }
    /** */ ExpressionNode key;
    /** */ ExpressionNode value;
    mixin OpEquals;
}

///
final class KeyValuePairs : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(keyValuePairs));
    }
    /** */ KeyValuePair[] keyValuePairs;
    mixin OpEquals;
}

///
final class LabeledStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifier, declarationOrStatement));
    }
    Token identifier;
    /** */ DeclarationOrStatement declarationOrStatement;
    mixin OpEquals;
}

///
final class LambdaExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifier, returnType, parameters, functionAttributes,
            assignExpression));
    }
    /** */ IdType functionType;
    /** */ Token identifier;
    /** */ Parameters parameters;
    /** */ FunctionAttribute[] functionAttributes;
    /** */ ExpressionNode assignExpression;
    /** */ Type returnType;
    mixin OpEquals;
}

///
final class LastCatch : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(statementNoCaseNoDefault));
    }
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    size_t line;
    size_t column;
    mixin OpEquals;
}

///
final class LinkageAttribute : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifier, identifierChain));
    }
    /** */ Token identifier;
    /** */ bool hasPlusPlus;
    /** */ IdentifierChain identifierChain;
    mixin OpEquals;
}

///
final class MemberFunctionAttribute : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(atAttribute));
    }
    /** */ IdType tokenType;
    /** */ AtAttribute atAttribute;
    mixin OpEquals;
}

///
final class MixinDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(mixinExpression, templateMixinExpression));
    }
    /** */ MixinExpression mixinExpression;
    /** */ TemplateMixinExpression templateMixinExpression;
    mixin OpEquals;
}

///
final class MixinExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(assignExpression));
    }
    /** */ ExpressionNode assignExpression;
    mixin OpEquals;
}

///
final class MixinTemplateDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(templateDeclaration));
    }
    /** */ TemplateDeclaration templateDeclaration;
    mixin OpEquals;
}

///
final class MixinTemplateName : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(symbol, typeofExpression, identifierOrTemplateChain));
    }
    /** */ Symbol symbol;
    /** */ IdentifierOrTemplateChain identifierOrTemplateChain;
    /** */ TypeofExpression typeofExpression;
    mixin OpEquals;
}

///
final class Module : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(scriptLine, moduleDeclaration, declarations));
    }
    /** */ Token scriptLine;
    /** */ ModuleDeclaration moduleDeclaration;
    /** */ Declaration[] declarations;
    mixin OpEquals;
}

///
final class ModuleDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(moduleName, deprecated_));
    }
    /** */ Deprecated deprecated_;
    /** */ IdentifierChain moduleName;
    /** */ size_t startLocation;
    /** */ size_t endLocation;
    /** */ string comment;
    mixin OpEquals;
}


///
final class MulExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    /** */ IdType operator;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class NewAnonClassExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(allocatorArguments, constructorArguments,
            baseClassList, structBody));
    }
    /** */ Arguments allocatorArguments;
    /** */ Arguments constructorArguments;
    /** */ BaseClassList baseClassList;
    /** */ StructBody structBody;
    mixin OpEquals;
}

///
final class NewExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(newAnonClassExpression, type, arguments,
            assignExpression));
    }
    /** */ Type type;
    /** */ NewAnonClassExpression newAnonClassExpression;
    /** */ Arguments arguments;
    /** */ ExpressionNode assignExpression;
    mixin OpEquals;
}


///
final class StatementNoCaseNoDefault : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(labeledStatement, blockStatement, ifStatement,
            whileStatement, doStatement, forStatement, foreachStatement,
            switchStatement, finalSwitchStatement, continueStatement,
            breakStatement, returnStatement, gotoStatement, withStatement,
            synchronizedStatement, tryStatement, throwStatement,
            scopeGuardStatement, asmStatement, conditionalStatement,
            staticAssertStatement, versionSpecification, debugSpecification,
            expressionStatement));
    }
    /** */ LabeledStatement labeledStatement;
    /** */ BlockStatement blockStatement;
    /** */ IfStatement ifStatement;
    /** */ WhileStatement whileStatement;
    /** */ DoStatement doStatement;
    /** */ ForStatement forStatement;
    /** */ ForeachStatement foreachStatement;
    /** */ SwitchStatement switchStatement;
    /** */ FinalSwitchStatement finalSwitchStatement;
    /** */ ContinueStatement continueStatement;
    /** */ BreakStatement breakStatement;
    /** */ ReturnStatement returnStatement;
    /** */ GotoStatement gotoStatement;
    /** */ WithStatement withStatement;
    /** */ SynchronizedStatement synchronizedStatement;
    /** */ TryStatement tryStatement;
    /** */ ThrowStatement throwStatement;
    /** */ ScopeGuardStatement scopeGuardStatement;
    /** */ AsmStatement asmStatement;
    /** */ ConditionalStatement conditionalStatement;
    /** */ StaticAssertStatement staticAssertStatement;
    /** */ VersionSpecification versionSpecification;
    /** */ DebugSpecification debugSpecification;
    /** */ ExpressionStatement expressionStatement;
    /** */ size_t startLocation;
    /** */ size_t endLocation;
    mixin OpEquals;
}

///
final class NonVoidInitializer : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(assignExpression, arrayInitializer,
            structInitializer));
    }
    /** */ ExpressionNode assignExpression;
    /** */ ArrayInitializer arrayInitializer;
    /** */ StructInitializer structInitializer;

    mixin OpEquals;
}

///
final class Operands : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(operands));
    }
    /** */ ExpressionNode[] operands;
    mixin OpEquals;
}

///
final class OrExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class OrOrExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class OutStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(parameter, blockStatement));
    }
    /** */ size_t outTokenLocation;
    /** */ Token parameter;
    /** */ BlockStatement blockStatement;
    mixin OpEquals;
}

///
final class Parameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type, name, default_));
    }

    /** */ IdType[] parameterAttributes;
    /** */ Type type;
    /** */ Token name;
    /** */ bool vararg;
    /** */ ExpressionNode default_;
    /** */ TypeSuffix[] cstyle;

    mixin OpEquals;
}

///
final class Parameters : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(parameters));
    }

    /** */ Parameter[] parameters;
    /** */ bool hasVarargs;
    mixin OpEquals;
}

///
final class Postblit : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(functionBody));
    }
    /** */ FunctionBody functionBody;
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    mixin OpEquals;
}

///
final class PowExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class PragmaDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(pragmaExpression));
    }
    /** */ PragmaExpression pragmaExpression;
    mixin OpEquals;
}

///
final class PragmaExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifier, argumentList));
    }
    /** */ Token identifier;
    /** */ ArgumentList argumentList;
    mixin OpEquals;
}

///
final class PrimaryExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(basicType, typeConstructor, type, primary,
            typeofExpression, typeidExpression, arrayLiteral, assocArrayLiteral,
            expression, dot, identifierOrTemplateInstance, isExpression,
            lambdaExpression, functionLiteralExpression, traitsExpression,
            mixinExpression, importExpression, vector, arguments));
    }
    /** */ Token dot;
    /** */ Token primary;
    /** */ IdentifierOrTemplateInstance identifierOrTemplateInstance;
    /** */ Token basicType;
    /** */ TypeofExpression typeofExpression;
    /** */ TypeidExpression typeidExpression;
    /** */ ArrayLiteral arrayLiteral;
    /** */ AssocArrayLiteral assocArrayLiteral;
    /** */ Expression expression;
    /** */ IsExpression isExpression;
    /** */ LambdaExpression lambdaExpression;
    /** */ FunctionLiteralExpression functionLiteralExpression;
    /** */ TraitsExpression traitsExpression;
    /** */ MixinExpression mixinExpression;
    /** */ ImportExpression importExpression;
    /** */ Vector vector;
    /** */ Type type;
    /** */ Token typeConstructor;
    /** */ Arguments arguments;
    mixin OpEquals;
}

///
final class Register : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifier, intLiteral));
    }
    /** */ Token identifier;
    /** */ Token intLiteral;
    /** */ bool hasIntegerLiteral;
    mixin OpEquals;
}

///
final class RelExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    /** */ IdType operator;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class ReturnStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(expression));
    }
    /** */ Expression expression;
    /** */ size_t startLocation;
    /** */ size_t endLocation;
    mixin OpEquals;
}

///
final class ScopeGuardStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifier, statementNoCaseNoDefault));
    }
    /** */ Token identifier;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    mixin OpEquals;
}

///
final class SharedStaticConstructor : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(functionBody));
    }
    /** */ FunctionBody functionBody;
    /** */ size_t location;
    /** */ string comment;
    mixin OpEquals;
}

///
final class SharedStaticDestructor : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(functionBody));
    }
    /** */ FunctionBody functionBody;
    /** */ size_t location;
    /** */ string comment;
    mixin OpEquals;
}

///
final class ShiftExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    /** */ IdType operator;
    mixin BinaryExpressionBody;
    mixin OpEquals;
}

///
final class SingleImport : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(rename, identifierChain));
    }
    /** */ Token rename;
    /** */ IdentifierChain identifierChain;
    mixin OpEquals;
}

///
final class SliceExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(unaryExpression, lower, upper));
    }
    /** */ UnaryExpression unaryExpression;
    /** */ ExpressionNode lower;
    /** */ ExpressionNode upper;
    mixin OpEquals;
}

///
final class Statement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(statementNoCaseNoDefault, caseStatement,
            caseRangeStatement, defaultStatement));
    }
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    /** */ CaseStatement caseStatement;
    /** */ CaseRangeStatement caseRangeStatement;
    /** */ DefaultStatement defaultStatement;
    mixin OpEquals;
}

///
final class StaticAssertDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(staticAssertStatement));
    }
    /** */ StaticAssertStatement staticAssertStatement;
    mixin OpEquals;
}

///
final class StaticAssertStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(assertExpression));
    }
    /** */ AssertExpression assertExpression;
    mixin OpEquals;
}

///
final class StaticConstructor : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(functionBody));
    }
    /** */ FunctionBody functionBody;
    /** */ size_t location;
    /** */ string comment;
    mixin OpEquals;
}

///
final class StaticDestructor : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(functionBody));
    }
    /** */ FunctionBody functionBody;
    /** */ size_t location;
    /** */ string comment;
    mixin OpEquals;
}

///
final class StaticIfCondition : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(assignExpression));
    }
    /** */ ExpressionNode assignExpression;
    mixin OpEquals;
}

///
final class StorageClass : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(token, alignAttribute, linkageAttribute,
            atAttribute, deprecated_));
    }
    /** */ AlignAttribute alignAttribute;
    /** */ LinkageAttribute linkageAttribute;
    /** */ AtAttribute atAttribute;
    /** */ Deprecated deprecated_;
    /** */ Token token;
    mixin OpEquals;
}

///
final class StructBody : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(declarations));
    }

    /**
     * Byte position of the opening brace
     */
    size_t startLocation;

    /**
     * Byte position of the closing brace
     */
    size_t endLocation;
    /** */ Declaration[] declarations;
    mixin OpEquals;
}

///
final class StructDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(templateParameters, constraint, structBody));
    }
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ StructBody structBody;
    /** */ string comment;
    mixin OpEquals;
}

///
final class StructInitializer : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(structMemberInitializers));
    }
    /** */ StructMemberInitializers structMemberInitializers;
    /** */ size_t startLocation;
    /** */ size_t endLocation;
    mixin OpEquals;
}

///
final class StructMemberInitializer : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifier, nonVoidInitializer));
    }
    /** */ Token identifier;
    /** */ NonVoidInitializer nonVoidInitializer;
    mixin OpEquals;
}

///
final class StructMemberInitializers : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(structMemberInitializers));
    }
    /** */ StructMemberInitializer[] structMemberInitializers;
    mixin OpEquals;
}

///
final class SwitchStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(expression, statement));
    }
    /** */ Expression expression;
    /** */ Statement statement;
    mixin OpEquals;
}

///
final class Symbol : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifierOrTemplateChain));
    }

    /** */ IdentifierOrTemplateChain identifierOrTemplateChain;
    /** */ bool dot;
    mixin OpEquals;
}

///
final class SynchronizedStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(expression, statementNoCaseNoDefault));
    }
    /** */ Expression expression;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    mixin OpEquals;
}

///
final class TemplateAliasParameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type, identifier, colonType, colonExpression,
            assignType, assignExpression));
    }
    /** */ Type type;
    /** */ Token identifier;
    /** */ Type colonType;
    /** */ ExpressionNode colonExpression;
    /** */ Type assignType;
    /** */ ExpressionNode assignExpression;
    mixin OpEquals;
}

///
final class TemplateArgument : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type, assignExpression));
    }
    /** */ Type type;
    /** */ ExpressionNode assignExpression;
    mixin OpEquals;
}

///
final class TemplateArgumentList : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(items));
    }
    /** */ TemplateArgument[] items;
    mixin OpEquals;
}

///
final class TemplateArguments : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(templateArgumentList, templateSingleArgument));
    }
    /** */ TemplateArgumentList templateArgumentList;
    /** */ TemplateSingleArgument templateSingleArgument;
    mixin OpEquals;
}

///
final class TemplateDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(name, templateParameters, constraint,
            declarations));
    }
    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ Declaration[] declarations;
    /** */ string comment;
    /**
     * Byte position of the opening brace
     */
    size_t startLocation;

    /**
     * Byte position of the closing brace
     */
    size_t endLocation;
    mixin OpEquals;
}

///
final class TemplateInstance : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifier, templateArguments));
    }
    /** */ Token identifier;
    /** */ TemplateArguments templateArguments;
    mixin OpEquals;
}

///
final class TemplateMixinExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifier, templateArguments, mixinTemplateName));
    }
    /** */ Token identifier;
    /** */ TemplateArguments templateArguments;
    /** */ MixinTemplateName mixinTemplateName;
    mixin OpEquals;
}

///
final class TemplateParameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(templateTypeParameter, templateValueParameter,
            templateAliasParameter, templateTupleParameter,
            templateThisParameter));
    }
    /** */ TemplateTypeParameter templateTypeParameter;
    /** */ TemplateValueParameter templateValueParameter;
    /** */ TemplateAliasParameter templateAliasParameter;
    /** */ TemplateTupleParameter templateTupleParameter;
    /** */ TemplateThisParameter templateThisParameter;
    mixin OpEquals;
}

///
final class TemplateParameterList : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(items));
    }
    /** */ TemplateParameter[] items;
    mixin OpEquals;
}

///
final class TemplateParameters : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(templateParameterList));
    }
    /** */ TemplateParameterList templateParameterList;
    mixin OpEquals;
}

///
final class TemplateSingleArgument : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(token));
    }
    /** */ Token token;
    mixin OpEquals;
}

///
final class TemplateThisParameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(templateTypeParameter));
    }
    /** */ TemplateTypeParameter templateTypeParameter;
    mixin OpEquals;
}

///
final class TemplateTupleParameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifier));
    }
    /** */ Token identifier;
    mixin OpEquals;
}

///
final class TemplateTypeParameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifier, colonType, assignType));
    }
    /** */ Token identifier;
    /** */ Type colonType;
    /** */ Type assignType;
    mixin OpEquals;
}

///
final class TemplateValueParameter : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type, identifier, assignExpression,
            templateValueParameterDefault));
    }
    /** */ Type type;
    /** */ Token identifier;
    /** */ ExpressionNode assignExpression;
    /** */ TemplateValueParameterDefault templateValueParameterDefault;
    mixin OpEquals;
}

///
final class TemplateValueParameterDefault : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(token, assignExpression));
    }
    /** */ ExpressionNode assignExpression;
    /** */ Token token;
    mixin OpEquals;
}

///
final class TernaryExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(orOrExpression, expression, ternaryExpression));
    }
    /** */ ExpressionNode orOrExpression;
    /** */ ExpressionNode expression;
    /** */ ExpressionNode ternaryExpression;
    /// Store this so that we know where the ':' is
    Token colon;
    mixin OpEquals;
}

///
final class ThrowStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(expression));
    }
    /** */ Expression expression;
    mixin OpEquals;
}

///
final class TraitsExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(identifier, templateArgumentList));
    }
    /** */ Token identifier;
    /** */ TemplateArgumentList templateArgumentList;
    mixin OpEquals;
}

///
final class TryStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(declarationOrStatement, catches, finally_));
    }
    /** */ DeclarationOrStatement declarationOrStatement;
    /** */ Catches catches;
    /** */ Finally finally_;
    mixin OpEquals;
}

///
final class Type : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type2, typeSuffixes));
    }

    /** */ IdType[] typeConstructors;
    /** */ TypeSuffix[] typeSuffixes;
    /** */ Type2 type2;
    mixin OpEquals;
}

///
final class Type2 : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(symbol, typeofExpression,
            identifierOrTemplateChain, type, vector));
    }

    /** */ IdType builtinType;
    /** */ Symbol symbol;
    /** */ TypeofExpression typeofExpression;
    /** */ IdentifierOrTemplateChain identifierOrTemplateChain;
    /** */ IdType typeConstructor;
    /** */ Type type;
    /** */ Vector vector;
    mixin OpEquals;
}

///
final class TypeSpecialization : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(token, type));
    }
    /** */ Token token;
    /** */ Type type;
    mixin OpEquals;
}

///
final class TypeSuffix : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type, low, high, delegateOrFunction, parameters,
            memberFunctionAttributes));
    }

    /** */ Token delegateOrFunction;
    /** */ Token star;
    /** */ bool array;
    /** */ Type type;
    /** */ ExpressionNode low;
    /** */ ExpressionNode high;
    /** */ Parameters parameters;
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    mixin OpEquals;
}

///
final class TypeidExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type, expression));
    }
    /** */ Type type;
    /** */ Expression expression;
    mixin OpEquals;
}

///
final class TypeofExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(expression, return_));
    }
    /** */ Expression expression;
    /** */ Token return_;
    mixin OpEquals;
}

///
final class UnaryExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        // TODO prefix, postfix, unary
        mixin (visitIfNotNull!(primaryExpression, newExpression,
            deleteExpression, castExpression, functionCallExpression, argumentList,
            unaryExpression, type, identifierOrTemplateInstance, assertExpression,
            sliceExpression, indexExpression));
    }

    /** */ Type type;
    /** */ PrimaryExpression primaryExpression;
    /** */ Token prefix;
    /** */ Token suffix;
    /** */ UnaryExpression unaryExpression;
    /** */ NewExpression newExpression;
    /** */ DeleteExpression deleteExpression;
    /** */ CastExpression castExpression;
    /** */ FunctionCallExpression functionCallExpression;
    /** */ ArgumentList argumentList;
    /** */ IdentifierOrTemplateInstance identifierOrTemplateInstance;
    /** */ AssertExpression assertExpression;
    /** */ SliceExpression sliceExpression;
    /** */ IndexExpression indexExpression;
    mixin OpEquals;
}

///
final class UnionDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(name, templateParameters, constraint, structBody));
    }

    /** */ Token name;
    /** */ TemplateParameters templateParameters;
    /** */ Constraint constraint;
    /** */ StructBody structBody;
    /** */ string comment;
    mixin OpEquals;
}

///
final class Unittest : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(blockStatement));
    }
    /** */ BlockStatement blockStatement;
    /** */ string comment;
    mixin OpEquals;
}

///
final class VariableDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(attributes, storageClasses, type, declarators, autoDeclaration));
    }
    /** */ Type type;
    /** */ Declarator[] declarators;
    /** */ StorageClass[] storageClasses;
    /** */ Attribute[] attributes;
    /** */ AutoDeclaration autoDeclaration;
    /** */ string comment;
    mixin OpEquals;
}

///
final class Vector : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(type));
    }
    /** */ Type type;
    mixin OpEquals;
}

///
final class VersionCondition : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(token));
    }
    /** */ size_t versionIndex;
    /** */ Token token;
    mixin OpEquals;
}

///
final class VersionSpecification : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(token));
    }
    /** */ Token token;
    mixin OpEquals;
}

///
final class WhileStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(expression, declarationOrStatement));
    }

    /** */ Expression expression;
    /** */ DeclarationOrStatement declarationOrStatement;
    /** */ size_t startIndex;
    mixin OpEquals;
}

///
final class WithStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(expression, statementNoCaseNoDefault));
    }

    /** */ Expression expression;
    /** */ StatementNoCaseNoDefault statementNoCaseNoDefault;
    mixin OpEquals;
}

///
final class XorExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(left, right));
    }
    mixin BinaryExpressionBody;
    mixin OpEquals;
}
