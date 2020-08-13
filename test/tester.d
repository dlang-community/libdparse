import std.array;
import std.exception;
import std.file;
import std.getopt;
import std.stdio;
import dparse.ast;
import dparse.lexer;
import dparse.parser;
import dparse.astprinter;

int errorCount = 0;
int warningCount = 0;

void messageFunction(string fileName, size_t line, size_t column,
    string message, bool isError)
{
    if (isError)
    {
        errorCount++;
        stderr.writefln("%s(%d:%d)[error]: %s", fileName, line, column, message);
    }
    else
    {
        stderr.writefln("%s(%d:%d)[warn ]: %s", fileName, line, column, message);
        warningCount++;
    }
}

void testTokenChecks()
{
    foreach (IdType i; 0 .. IdType.max)
    {
        switch (i)
        {
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
            assert (isBasicType(i));
            assert (!isNumberLiteral(i));
            assert (!isIntegerLiteral(i));
            assert (!isOperator(i));
            assert (!isKeyword(i));
            assert (!isStringLiteral(i));
            assert (!isProtection(i));
            break;
        case tok!"doubleLiteral":
        case tok!"floatLiteral":
        case tok!"idoubleLiteral":
        case tok!"ifloatLiteral":
        case tok!"realLiteral":
        case tok!"irealLiteral":
            assert (!isBasicType(i));
            assert (isNumberLiteral(i));
            assert (!isIntegerLiteral(i));
            assert (!isOperator(i));
            assert (!isKeyword(i));
            assert (!isStringLiteral(i));
            assert (!isProtection(i));
            break;
        case tok!"intLiteral":
        case tok!"longLiteral":
        case tok!"uintLiteral":
        case tok!"ulongLiteral":
            assert (!isBasicType(i));
            assert (isNumberLiteral(i));
            assert (isIntegerLiteral(i));
            assert (!isOperator(i));
            assert (!isKeyword(i));
            assert (!isStringLiteral(i));
            assert (!isProtection(i));
            break;
        case tok!",":
        case tok!".":
        case tok!"..":
        case tok!"...":
        case tok!"/":
        case tok!"/=":
        case tok!"!":
        case tok!"!<":
        case tok!"!<=":
        case tok!"!<>":
        case tok!"!<>=":
        case tok!"!=":
        case tok!"!>":
        case tok!"!>=":
        case tok!"$":
        case tok!"%":
        case tok!"%=":
        case tok!"&":
        case tok!"&&":
        case tok!"&=":
        case tok!"(":
        case tok!")":
        case tok!"*":
        case tok!"*=":
        case tok!"+":
        case tok!"++":
        case tok!"+=":
        case tok!"-":
        case tok!"--":
        case tok!"-=":
        case tok!":":
        case tok!";":
        case tok!"<":
        case tok!"<<":
        case tok!"<<=":
        case tok!"<=":
        case tok!"<>":
        case tok!"<>=":
        case tok!"=":
        case tok!"==":
        case tok!"=>":
        case tok!">":
        case tok!">=":
        case tok!">>":
        case tok!">>=":
        case tok!">>>":
        case tok!">>>=":
        case tok!"?":
        case tok!"@":
        case tok!"[":
        case tok!"]":
        case tok!"^":
        case tok!"^=":
        case tok!"^^":
        case tok!"^^=":
        case tok!"{":
        case tok!"|":
        case tok!"|=":
        case tok!"||":
        case tok!"}":
        case tok!"~":
        case tok!"~=":
            assert (!isBasicType(i));
            assert (!isNumberLiteral(i));
            assert (!isIntegerLiteral(i));
            assert (isOperator(i));
            assert (!isKeyword(i));
            assert (!isStringLiteral(i));
            assert (!isProtection(i));
            break;
        case tok!"abstract":
        case tok!"alias":
        case tok!"align":
        case tok!"asm":
        case tok!"assert":
        case tok!"auto":
        case tok!"break":
        case tok!"case":
        case tok!"cast":
        case tok!"catch":
        case tok!"class":
        case tok!"const":
        case tok!"continue":
        case tok!"debug":
        case tok!"default":
        case tok!"delegate":
        case tok!"delete":
        case tok!"deprecated":
        case tok!"do":
        case tok!"else":
        case tok!"enum":
        case tok!"extern":
        case tok!"false":
        case tok!"final":
        case tok!"finally":
        case tok!"for":
        case tok!"foreach":
        case tok!"foreach_reverse":
        case tok!"function":
        case tok!"goto":
        case tok!"if":
        case tok!"immutable":
        case tok!"import":
        case tok!"in":
        case tok!"inout":
        case tok!"interface":
        case tok!"invariant":
        case tok!"is":
        case tok!"lazy":
        case tok!"macro":
        case tok!"mixin":
        case tok!"module":
        case tok!"new":
        case tok!"nothrow":
        case tok!"null":
        case tok!"out":
        case tok!"override":
        case tok!"pragma":
        case tok!"pure":
        case tok!"ref":
        case tok!"return":
        case tok!"scope":
        case tok!"shared":
        case tok!"static":
        case tok!"struct":
        case tok!"super":
        case tok!"switch":
        case tok!"synchronized":
        case tok!"template":
        case tok!"this":
        case tok!"throw":
        case tok!"true":
        case tok!"try":
        case tok!"typedef":
        case tok!"typeid":
        case tok!"typeof":
        case tok!"union":
        case tok!"unittest":
        case tok!"version":
        case tok!"while":
        case tok!"with":
        case tok!"__DATE__":
        case tok!"__EOF__":
        case tok!"__FILE__":
        case tok!"__FILE_FULL_PATH__":
        case tok!"__FUNCTION__":
        case tok!"__gshared":
        case tok!"__LINE__":
        case tok!"__MODULE__":
        case tok!"__parameters":
        case tok!"__PRETTY_FUNCTION__":
        case tok!"__TIME__":
        case tok!"__TIMESTAMP__":
        case tok!"__traits":
        case tok!"__vector":
        case tok!"__VENDOR__":
        case tok!"__VERSION__":
            assert (!isBasicType(i));
            assert (!isNumberLiteral(i));
            assert (!isIntegerLiteral(i));
            assert (!isOperator(i));
            assert (isKeyword(i));
            assert (!isStringLiteral(i));
            assert (!isProtection(i));
            break;
        case tok!"dstringLiteral":
        case tok!"stringLiteral":
        case tok!"wstringLiteral":
            assert (!isBasicType(i));
            assert (!isNumberLiteral(i));
            assert (!isIntegerLiteral(i));
            assert (!isOperator(i));
            assert (!isKeyword(i));
            assert (isStringLiteral(i));
            assert (!isProtection(i));
            break;
        case tok!"export":
        case tok!"package":
        case tok!"private":
        case tok!"public":
        case tok!"protected":
            assert (!isBasicType(i));
            assert (!isNumberLiteral(i));
            assert (!isIntegerLiteral(i));
            assert (!isOperator(i));
            assert (isKeyword(i));
            assert (!isStringLiteral(i));
            assert (isProtection(i));
            break;
        default:
            assert (!isBasicType(i));
            assert (!isNumberLiteral(i));
            assert (!isIntegerLiteral(i));
            assert (!isOperator(i));
            assert (!isKeyword(i));
            assert (!isStringLiteral(i));
            assert (!isProtection(i));
            break;
        }
    }
}

int main(string[] args)
{
    import dparse.rollback_allocator : RollbackAllocator;

    version (D_Coverage) testTokenChecks();

    bool ast;
    getopt(args, "ast", &ast);

    enforce(args.length > 1, "Must specifiy at least one D file");
    auto printer = new XMLPrinter;
    printer.output = stdout;
    foreach (arg; args[1 .. $])
    {
        auto f = File(arg);
        immutable ulong fileSize = f.size();
        ubyte[] fileBytes = new ubyte[](fileSize);
        enforce(f.rawRead(fileBytes).length == fileSize);
        StringCache cache = StringCache(fileSize.optimalBucketCount);
        LexerConfig config;
        config.stringBehavior = StringBehavior.source;
        config.fileName = arg;
        const(Token)[] tokens = getTokensForParser(fileBytes, config, &cache);
        RollbackAllocator rba;
        auto mod = parseModule(ParserConfig(tokens, arg, &rba, &messageFunction));
        if (ast && mod !is null)
            printer.visit(mod);
    }
    if (!ast)
        writefln("Finished parsing with %d errors and %d warnings.",
                errorCount, warningCount);
    return errorCount == 0 ? 0 : 1;
}
