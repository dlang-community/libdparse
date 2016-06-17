module dparse.types;
import dparse.ast;
import dparse.lexer: Token;
import std.traits: TypeTuple;

alias DeclarationTypes = TypeTuple!(AliasDeclaration,
                                                                        AliasThisDeclaration,
                                                                        AnonymousEnumDeclaration,
                                                                        AttributeDeclaration,
                                                                        ClassDeclaration,
                                                                        ConditionalDeclaration,
                                                                        Constructor,
                                                                        DebugSpecification,
                                                                        Destructor,
                                                                        EnumDeclaration,
                                                                        EponymousTemplateDeclaration,
                                                                        FunctionDeclaration,
                                                                        ImportDeclaration,
                                                                        InterfaceDeclaration,
                                                                        Invariant,
                                                                        MixinDeclaration,
                                                                        MixinTemplateDeclaration,
                                                                        Postblit,
                                                                        PragmaDeclaration,
                                                                        SharedStaticConstructor,
                                                                        SharedStaticDestructor,
                                                                        StaticAssertDeclaration,
                                                                        StaticConstructor,
                                                                        StaticDestructor,
                                                                        StructDeclaration,
                                                                        TemplateDeclaration,
                                                                        UnionDeclaration,
                                                                        Unittest,
                                                                        VariableDeclaration,
                                                                        VersionSpecification);

alias NodeTypes =
    TypeTuple!(
                         AddExpression,
                         AliasDeclaration,
                         AliasInitializer,
                         AliasThisDeclaration,
                         AlignAttribute,
                         AndAndExpression,
                         AndExpression,
                         AnonymousEnumDeclaration,
                         AnonymousEnumMember,
                         ArgumentList,
                         Arguments,
                         ArrayInitializer,
                         ArrayLiteral,
                         ArrayMemberInitialization,
                         AsmAddExp,
                         AsmBrExp,
                         AsmEqualExp,
                         AsmExp,
                         AsmInstruction,
                         AsmLogAndExp,
                         AsmLogOrExp,
                         AsmMulExp,
                         AsmOrExp,
                         AsmPrimaryExp,
                         AsmRelExp,
                         AsmShiftExp,
                         AsmStatement,
                         AsmTypePrefix,
                         AsmUnaExp,
                         AsmXorExp,
                         AssertExpression,
                         AssignExpression,
                         AssocArrayLiteral,
                         AtAttribute,
                         Attribute,
                         AttributeDeclaration,
                         AutoDeclaration,
                         BlockStatement,
                         BodyStatement,
                         BreakStatement,
                         BaseClass,
                         BaseClassList,
                         CaseRangeStatement,
                         CaseStatement,
                         CastExpression,
                         CastQualifier,
                         Catch,
                         Catches,
                         ClassDeclaration,
                         CmpExpression,
                         CompileCondition,
                         ConditionalDeclaration,
                         ConditionalStatement,
                         Constraint,
                         Constructor,
                         ContinueStatement,
                         DebugCondition,
                         DebugSpecification,
                         Declaration,
                         DeclarationOrStatement,
                         DeclarationsAndStatements,
                         Declarator,
                         DefaultStatement,
                         DeleteExpression,
                         DeleteStatement,
                         Deprecated,
                         Destructor,
                         DoStatement,
                         EnumBody,
                         EnumDeclaration,
                         EnumMember,
                         EponymousTemplateDeclaration,
                         EqualExpression,
                         Expression,
                         ExpressionStatement,
                         FinalSwitchStatement,
                         Finally,
                         ForStatement,
                         ForeachStatement,
                         ForeachType,
                         ForeachTypeList,
                         FunctionAttribute,
                         FunctionBody,
                         FunctionCallExpression,
                         FunctionDeclaration,
                         FunctionLiteralExpression,
                         GotoStatement,
                         IdentifierChain,
                         IdentifierList,
                         IdentifierOrTemplateChain,
                         IdentifierOrTemplateInstance,
                         IdentityExpression,
                         IfStatement,
                         ImportBind,
                         ImportBindings,
                         ImportDeclaration,
                         ImportExpression,
                         IndexExpression,
                         InExpression,
                         InStatement,
                         Initialize,
                         Initializer,
                         InterfaceDeclaration,
                         Invariant,
                         IsExpression,
                         KeyValuePair,
                         KeyValuePairs,
                         LabeledStatement,
                         LastCatch,
                         LinkageAttribute,
                         MemberFunctionAttribute,
                         MixinDeclaration,
                         MixinExpression,
                         MixinTemplateDeclaration,
                         MixinTemplateName,
                         Module,
                         ModuleDeclaration,
                         MulExpression,
                         NewAnonClassExpression,
                         NewExpression,
                         NonVoidInitializer,
                         Operands,
                         OrExpression,
                         OrOrExpression,
                         OutStatement,
                         Parameter,
                         Parameters,
                         Postblit,
                         PowExpression,
                         PragmaDeclaration,
                         PragmaExpression,
                         PrimaryExpression,
                         Register,
                         RelExpression,
                         ReturnStatement,
                         ScopeGuardStatement,
                         SharedStaticConstructor,
                         SharedStaticDestructor,
                         ShiftExpression,
                         SingleImport,
                         Index,
                         Statement,
                         StatementNoCaseNoDefault,
                         StaticAssertDeclaration,
                         StaticAssertStatement,
                         StaticConstructor,
                         StaticDestructor,
                         StaticIfCondition,
                         StorageClass,
                         StructBody,
                         StructDeclaration,
                         StructInitializer,
                         StructMemberInitializer,
                         StructMemberInitializers,
                         SwitchStatement,
                         Symbol,
                         SynchronizedStatement,
                         TemplateAliasParameter,
                         TemplateArgument,
                         TemplateArgumentList,
                         TemplateArguments,
                         TemplateDeclaration,
                         TemplateInstance,
                         TemplateMixinExpression,
                         TemplateParameter,
                         TemplateParameterList,
                         TemplateParameters,
                         TemplateSingleArgument,
                         TemplateThisParameter,
                         TemplateTupleParameter,
                         TemplateTypeParameter,
                         TemplateValueParameter,
                         TemplateValueParameterDefault,
                         TernaryExpression,
                         ThrowStatement,
                         TraitsExpression,
                         TryStatement,
                         Type,
                         Type2,
                         TypeSpecialization,
                         TypeSuffix,
                         TypeidExpression,
                         TypeofExpression,
                         UnaryExpression,
                         UnionDeclaration,
                         Unittest,
                         VariableDeclaration,
                         Vector,
                         VersionCondition,
                         VersionSpecification,
                         WhileStatement,
                         WithStatement,
                         XorExpression);
