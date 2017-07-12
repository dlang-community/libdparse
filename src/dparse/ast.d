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
 */

module dparse.ast;

import dparse.lexer;
import std.traits;
import std.algorithm;
import std.array;
import std.string;

private immutable uint[TypeInfo] typeMap;

shared static this()
{
    typeMap[typeid(AddExpression)] = 1;
    typeMap[typeid(AndAndExpression)] = 2;
    typeMap[typeid(AndExpression)] = 3;
    typeMap[typeid(AsmAddExp)] = 4;
    typeMap[typeid(AsmAndExp)] = 5;
    typeMap[typeid(AsmBrExp)] = 6;
    typeMap[typeid(AsmExp)] = 7;
    typeMap[typeid(AsmEqualExp)] = 8;
    typeMap[typeid(AsmLogAndExp)] = 9;
    typeMap[typeid(AsmLogOrExp)] = 10;
    typeMap[typeid(AsmMulExp)] = 11;
    typeMap[typeid(AsmOrExp)] = 12;
    typeMap[typeid(AsmRelExp)] = 13;
    typeMap[typeid(AsmUnaExp)] = 14;
    typeMap[typeid(AsmShiftExp)] = 15;
    typeMap[typeid(AsmXorExp)] = 16;
    typeMap[typeid(AssertExpression)] = 17;
    typeMap[typeid(AssignExpression)] = 18;
    typeMap[typeid(CmpExpression)] = 19;
    typeMap[typeid(DeleteExpression)] = 20;
    typeMap[typeid(EqualExpression)] = 21;
    typeMap[typeid(Expression)] = 22;
    typeMap[typeid(FunctionCallExpression)] = 23;
    typeMap[typeid(FunctionLiteralExpression)] = 24;
    typeMap[typeid(IdentityExpression)] = 25;
    typeMap[typeid(ImportExpression)] = 26;
    typeMap[typeid(IndexExpression)] = 27;
    typeMap[typeid(InExpression)] = 28;
    typeMap[typeid(IsExpression)] = 29;
    typeMap[typeid(MixinExpression)] = 30;
    typeMap[typeid(MulExpression)] = 31;
    typeMap[typeid(NewAnonClassExpression)] = 32;
    typeMap[typeid(NewExpression)] = 33;
    typeMap[typeid(OrExpression)] = 34;
    typeMap[typeid(OrOrExpression)] = 35;
    typeMap[typeid(PowExpression)] = 36;
    typeMap[typeid(PragmaExpression)] = 37;
    typeMap[typeid(PrimaryExpression)] = 38;
    typeMap[typeid(RelExpression)] = 39;
    typeMap[typeid(ShiftExpression)] = 40;
    typeMap[typeid(Index)] = 41;
    typeMap[typeid(TemplateMixinExpression)] = 42;
    typeMap[typeid(TernaryExpression)] = 43;
    typeMap[typeid(TraitsExpression)] = 44;
    typeMap[typeid(TypeidExpression)] = 45;
    typeMap[typeid(TypeofExpression)] = 46;
    typeMap[typeid(UnaryExpression)] = 47;
    typeMap[typeid(XorExpression)] = 48;
}

/**
 * Implements the $(LINK2 http://en.wikipedia.org/wiki/Visitor_pattern, Visitor Pattern)
 * for the various AST classes
 */
abstract class ASTVisitor
{
public:

    /** */
    void visit(const ExpressionNode n)
    {
        switch (typeMap[typeid(n)])
        {
        case 1: visit(cast(AddExpression) n); break;
        case 2: visit(cast(AndAndExpression) n); break;
        case 3: visit(cast(AndExpression) n); break;
        case 4: visit(cast(AsmAddExp) n); break;
        case 5: visit(cast(AsmAndExp) n); break;
        case 6: visit(cast(AsmBrExp) n); break;
        case 7: visit(cast(AsmExp) n); break;
        case 8: visit(cast(AsmEqualExp) n); break;
        case 9: visit(cast(AsmLogAndExp) n); break;
        case 10: visit(cast(AsmLogOrExp) n); break;
        case 11: visit(cast(AsmMulExp) n); break;
        case 12: visit(cast(AsmOrExp) n); break;
        case 13: visit(cast(AsmRelExp) n); break;
        case 14: visit(cast(AsmUnaExp) n); break;
        case 15: visit(cast(AsmShiftExp) n); break;
        case 16: visit(cast(AsmXorExp) n); break;
        case 17: visit(cast(AssertExpression) n); break;
        case 18: visit(cast(AssignExpression) n); break;
        case 19: visit(cast(CmpExpression) n); break;
        case 20: visit(cast(DeleteExpression) n); break;
        case 21: visit(cast(EqualExpression) n); break;
        case 22: visit(cast(Expression) n); break;
        case 23: visit(cast(FunctionCallExpression) n); break;
        case 24: visit(cast(FunctionLiteralExpression) n); break;
        case 25: visit(cast(IdentityExpression) n); break;
        case 26: visit(cast(ImportExpression) n); break;
        case 27: visit(cast(IndexExpression) n); break;
        case 28: visit(cast(InExpression) n); break;
        case 29: visit(cast(IsExpression) n); break;
        case 30: visit(cast(MixinExpression) n); break;
        case 31: visit(cast(MulExpression) n); break;
        case 32: visit(cast(NewAnonClassExpression) n); break;
        case 33: visit(cast(NewExpression) n); break;
        case 34: visit(cast(OrExpression) n); break;
        case 35: visit(cast(OrOrExpression) n); break;
        case 36: visit(cast(PowExpression) n); break;
        case 37: visit(cast(PragmaExpression) n); break;
        case 38: visit(cast(PrimaryExpression) n); break;
        case 39: visit(cast(RelExpression) n); break;
        case 40: visit(cast(ShiftExpression) n); break;
        case 41: visit(cast(Index) n); break;
        case 42: visit(cast(TemplateMixinExpression) n); break;
        case 43: visit(cast(TernaryExpression) n); break;
        case 44: visit(cast(TraitsExpression) n); break;
        case 45: visit(cast(TypeidExpression) n); break;
        case 46: visit(cast(TypeofExpression) n); break;
        case 47: visit(cast(UnaryExpression) n); break;
        case 48: visit(cast(XorExpression) n); break;
        default: assert(false, __MODULE__ ~ " has a bug");
        }
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
    /** */ void visit(const AutoDeclarationPart autoDeclarationPart) { autoDeclarationPart.accept(this); }
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
    /** */ void visit(const PragmaStatement pragmaStatement) { pragmaStatement.accept(this); }
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
    /** */ void visit(const Index index) { index.accept(this); }
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
        else static if (is(Unqual!(typeof(fields[0])) == Token))
            immutable visitIfNotNull = "if (" ~ fields[0].stringof ~ ` != tok!""` ~ ") visitor.visit(" ~ fields[0].stringof ~ ");\n";
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
        mixin (visitIfNotNull!(storageClasses, type, identifierList, initializers,
                               parameters, memberFunctionAttributes));
    }
    mixin OpEquals;
    /** */ StorageClass[] storageClasses;
    /** */ Type type;
    /** */ IdentifierList identifierList;
    /** */ AliasInitializer[] initializers;
    /** */ string comment;
    /** */ Parameters parameters;
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
}

///
final class AliasInitializer : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(name, templateParameters, storageClasses, type,
                functionLiteralExpression));
    }
    mixin OpEquals;
    /** */ Token name;
    /** */ StorageClass[] storageClasses;
    /** */ TemplateParameters templateParameters;
    /** */ Type type;
    /** */ FunctionLiteralExpression functionLiteralExpression;
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
    /** */ string comment;
}

///
final class AlignAttribute : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(assignExpression));
    }
    mixin OpEquals;
    /** */ ExpressionNode assignExpression;
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
        mixin (visitIfNotNull!(token, register, asmExp, identifierChain,
            segmentOverrideSuffix));
    }
    /** */ ExpressionNode asmExp;
    /** */ IdentifierChain identifierChain;
    /** */ Register register;
    /** */ ExpressionNode segmentOverrideSuffix;
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
    /** */ size_t line;
    /** */ size_t column;
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
        mixin (visitIfNotNull!(ternaryExpression, expression));
    }
    /** */ ExpressionNode ternaryExpression;
    /** */ ExpressionNode expression;
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
        mixin (visitIfNotNull!(templateInstance, argumentList));
    }
    /** */ ArgumentList argumentList;
    /** */ TemplateInstance templateInstance;
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
            alignAttribute, identifierChain, linkageAttribute));
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
        foreach (part; parts)
            visitor.visit(part);
    }
    /** */ AutoDeclarationPart[] parts;
    /** */ StorageClass[] storageClasses;
    /** */ string comment;
    mixin OpEquals;
}

final class AutoDeclarationPart : ASTNode
{
    override void accept(ASTVisitor visitor) const
    {
        if (templateParameters !is null)
            visitor.visit(templateParameters);
        visitor.visit(initializer);
    }

    /** */ Token identifier;
    /** */ Initializer initializer;
    /** */ TemplateParameters templateParameters;
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
        mixin (visitIfNotNull!(compileCondition, trueDeclarations, falseDeclarations));
    }
    /** */ CompileCondition compileCondition;
    /** */ Declaration[] trueDeclarations;
    /** */ Declaration[] falseDeclarations;
    /** */ bool hasElse;
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
    /** */ size_t location;
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

        foreach (attr; attributes)
            visitor.visit(attr);
        foreach (dec; declarations)
            visitor.visit(dec);
        foreach (Type; DeclarationTypes)
        {
            const(Type)* value = storage.peek!Type;
            if (value !is null)
            {
                static if (isArray!Type)
                    foreach (item; *(cast(Type*) value))
                        visitor.visit(item);
                else if (*value !is null)
                    visitor.visit(*(cast(Type*) value));
            }
        }
    }

    private import std.variant:Algebraic;
    private import std.typetuple:TypeTuple;

    alias DeclarationTypes = TypeTuple!(AliasDeclaration, AliasThisDeclaration,
        AnonymousEnumDeclaration, AttributeDeclaration,
        ClassDeclaration, ConditionalDeclaration, Constructor, DebugSpecification,
        Destructor, EnumDeclaration, EponymousTemplateDeclaration,
        FunctionDeclaration, ImportDeclaration, InterfaceDeclaration, Invariant,
        MixinDeclaration, MixinTemplateDeclaration, Postblit, PragmaDeclaration,
        SharedStaticConstructor, SharedStaticDestructor, StaticAssertDeclaration,
        StaticConstructor, StaticDestructor, StructDeclaration,
        TemplateDeclaration, UnionDeclaration, Unittest, VariableDeclaration,
        VersionSpecification);

    private Algebraic!(DeclarationTypes) storage;

    private static string generateProperty(string type, string name)
    {
        return "const(" ~ type ~ ") " ~ name ~ "() const @property { auto p = storage.peek!" ~ type ~ "; return p is null? null : *p;}\n"
            ~ "const(" ~ type ~ ") " ~ name ~ "(" ~ type ~ " node) @property { storage = node; return node; }";
    }

    /** */ Attribute[] attributes;
    /** */ Declaration[] declarations;

    mixin(generateProperty("AliasDeclaration", "aliasDeclaration"));
    mixin(generateProperty("AliasThisDeclaration", "aliasThisDeclaration"));
    mixin(generateProperty("AnonymousEnumDeclaration", "anonymousEnumDeclaration"));
    mixin(generateProperty("AttributeDeclaration", "attributeDeclaration"));
    mixin(generateProperty("ClassDeclaration", "classDeclaration"));
    mixin(generateProperty("ConditionalDeclaration", "conditionalDeclaration"));
    mixin(generateProperty("Constructor", "constructor"));
    mixin(generateProperty("DebugSpecification", "debugSpecification"));
    mixin(generateProperty("Destructor", "destructor"));
    mixin(generateProperty("EnumDeclaration", "enumDeclaration"));
    mixin(generateProperty("EponymousTemplateDeclaration", "eponymousTemplateDeclaration"));
    mixin(generateProperty("FunctionDeclaration", "functionDeclaration"));
    mixin(generateProperty("ImportDeclaration", "importDeclaration"));
    mixin(generateProperty("InterfaceDeclaration", "interfaceDeclaration"));
    mixin(generateProperty("Invariant", "invariant_"));
    mixin(generateProperty("MixinDeclaration", "mixinDeclaration"));
    mixin(generateProperty("MixinTemplateDeclaration", "mixinTemplateDeclaration"));
    mixin(generateProperty("Postblit", "postblit"));
    mixin(generateProperty("PragmaDeclaration", "pragmaDeclaration"));
    mixin(generateProperty("SharedStaticConstructor", "sharedStaticConstructor"));
    mixin(generateProperty("SharedStaticDestructor", "sharedStaticDestructor"));
    mixin(generateProperty("StaticAssertDeclaration", "staticAssertDeclaration"));
    mixin(generateProperty("StaticConstructor", "staticConstructor"));
    mixin(generateProperty("StaticDestructor", "staticDestructor"));
    mixin(generateProperty("StructDeclaration", "structDeclaration"));
    mixin(generateProperty("TemplateDeclaration", "templateDeclaration"));
    mixin(generateProperty("UnionDeclaration", "unionDeclaration"));
    mixin(generateProperty("Unittest", "unittest_"));
    mixin(generateProperty("VariableDeclaration", "variableDeclaration"));
    mixin(generateProperty("VersionSpecification", "versionSpecification"));

    override bool opEquals(Object other) const
    {
        auto otherDeclaration = cast(Declaration) other;
        if (otherDeclaration is null)
            return false;
        return attributes == otherDeclaration.attributes
            && declarations == otherDeclaration.declarations
            && storage == otherDeclaration.storage;
    }
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
        mixin (visitIfNotNull!(assignExpression));
    }
    /** */ ExpressionNode assignExpression;
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
        mixin (visitIfNotNull!(attributes, storageClasses, returnType, parameters,
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
        mixin (visitIfNotNull!(returnType, parameters, functionAttributes,
                memberFunctionAttributes, functionBody, assignExpression));
    }
    /** */ ExpressionNode assignExpression;
    /** */ FunctionAttribute[] functionAttributes;
    /** */ FunctionBody functionBody;
    /** */ IdType functionOrDelegate;
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    /** */ Parameters parameters;
    /** */ Token identifier;
    /** */ Type returnType;
    /** */ size_t line;
    /** */ size_t column;
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
    /** */ size_t startIndex;
    /** */ size_t endIndex;
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
final class Index : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(low, high));
    }
    /** */ ExpressionNode low;
    /** */ ExpressionNode high;
    mixin OpEquals;
}

///
final class IndexExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(unaryExpression, indexes));
    }
    /** */ UnaryExpression unaryExpression;
    /** */ Index[] indexes;
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
    /** */ Token identifier;
    /** */ DeclarationOrStatement declarationOrStatement;
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
    /** */ IdType classOrStruct;
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
            scopeGuardStatement, asmStatement, pragmaStatement,
            conditionalStatement, staticAssertStatement, versionSpecification,
            debugSpecification, expressionStatement));
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
    /** */ PragmaStatement pragmaStatement;
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
    /** */ size_t location;
    /** */ size_t line;
    /** */ size_t column;
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
final class PragmaStatement : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(pragmaExpression, statement, blockStatement));
    }
    /** */ PragmaExpression pragmaExpression;
    /** */ Statement statement;
    /** */ BlockStatement blockStatement;
    mixin OpEquals;
}

///
final class PrimaryExpression : ExpressionNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin(visitIfNotNull!(basicType, typeConstructor, type, primary,
                typeofExpression, typeidExpression, arrayLiteral, assocArrayLiteral,
                expression, dot, identifierOrTemplateInstance, isExpression,
                functionLiteralExpression,traitsExpression, mixinExpression,
                importExpression, vector, arguments));
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
        mixin (visitIfNotNull!(memberFunctionAttributes, functionBody));
    }
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    /** */ FunctionBody functionBody;
    /** */ size_t location;
    /** */ size_t line;
    /** */ size_t column;
    /** */ string comment;
    mixin OpEquals;
}

///
final class SharedStaticDestructor : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(memberFunctionAttributes, functionBody));
    }
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    /** */ FunctionBody functionBody;
    /** */ size_t location;
    /** */ size_t line;
    /** */ size_t column;
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
        mixin (visitIfNotNull!(memberFunctionAttributes, functionBody));
    }
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    /** */ FunctionBody functionBody;
    /** */ size_t location;
    /** */ size_t line;
    /** */ size_t column;
    /** */ string comment;
    mixin OpEquals;
}

///
final class StaticDestructor : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(memberFunctionAttributes, functionBody));
    }
    /** */ MemberFunctionAttribute[] memberFunctionAttributes;
    /** */ FunctionBody functionBody;
    /** */ size_t location;
    /** */ size_t line;
    /** */ size_t column;
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
    /** */ alias superOrThis = builtinType;
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
        mixin (visitIfNotNull!(primaryExpression, newExpression, deleteExpression,
            castExpression, functionCallExpression, argumentList, unaryExpression,
            type, identifierOrTemplateInstance, assertExpression, indexExpression));
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
    /** */ size_t location;
    /** */ size_t line;
    /** */ size_t column;
    mixin OpEquals;
}

///
final class VariableDeclaration : ASTNode
{
public:
    override void accept(ASTVisitor visitor) const
    {
        mixin (visitIfNotNull!(storageClasses, type, declarators, autoDeclaration));
    }
    /** */ Type type;
    /** */ Declarator[] declarators;
    /** */ StorageClass[] storageClasses;
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

unittest // issue #133
{
    import dparse.lexer, dparse.parser, dparse.rollback_allocator;
    ubyte[] src = cast(ubyte[])q{module test133; void main(){}};
    final class TkTest : ASTVisitor
    {
        alias visit = ASTVisitor.visit;
        override void visit(const Token t){assert(t.text.length);}
    }
    RollbackAllocator ra;
    LexerConfig cf = LexerConfig("", StringBehavior.source);
    StringCache ca = StringCache(16);
    Module m = parseModule(getTokensForParser(src, cf, &ca), "", &ra);
    TkTest t = new TkTest;
    t.visit(m);
}

