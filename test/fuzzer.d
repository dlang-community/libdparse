import dparse.lexer;
import std.random;
import std.stdio;

void main()
{
	foreach (i; 0 .. 100)
		writeRandomToken();
}

void writeRandomToken()
{
again:
	IdType i = cast(IdType) uniform(1, IdType.max);
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
	case tok!"abstract":
	case tok!"alias":
	case tok!"align":
	case tok!"asm":
	case tok!"assert":
	case tok!"auto":
	case tok!"body":
	case tok!"break":
	case tok!"case":
	case tok!"cast":
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
	case tok!"function":
	case tok!"goto":
	case tok!"immutable":
	case tok!"import":
	case tok!"in":
	case tok!"inout":
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
	case tok!"super":
	case tok!"template":
	case tok!"this":
	case tok!"throw":
	case tok!"true":
	case tok!"try":
	case tok!"typedef":
	case tok!"typeid":
	case tok!"typeof":
	case tok!"unittest":
	case tok!"version":
	case tok!"volatile":
	case tok!"__DATE__":
	case tok!"__EOF__":
	case tok!"__FILE__":
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
	case tok!"export":
	case tok!"package":
	case tok!"private":
	case tok!"public":
	case tok!"protected":
	case tok!"synchronized":
		write(str(i), " ");
		break;
	case tok!"class":
	case tok!"interface":
	case tok!"union":
	case tok!"struct":
		write(str(i), " ");
		if (uniform(0, 9) > 3)
			write("ident ");
		break;
	case tok!"switch":
	case tok!"if":
	case tok!"for":
	case tok!"foreach":
	case tok!"foreach_reverse":
	case tok!"while":
	case tok!"with":
	case tok!"catch":
		write(str(i), " ");
		if (uniform(0, 9) > 3)
			write("(");
		break;
	case tok!"doubleLiteral":
		write("1.0 ");
		break;
	case tok!"floatLiteral":
		write("1.0f ");
		break;
	case tok!"intLiteral":
		write("1 ");
		break;
	case tok!"longLiteral":
		write("1L ");
		break;
	case tok!"uintLiteral":
		write("1U ");
		break;
	case tok!"ulongLiteral":
		write("1UL ");
		break;
	case tok!"idoubleLiteral":
	case tok!"ifloatLiteral":
	case tok!"realLiteral":
	case tok!"irealLiteral":
		break;
	case tok!"dstringLiteral":
	case tok!"stringLiteral":
	case tok!"wstringLiteral":
		writeStringLiteral();
		break;
	case tok!"identifier":
		write("ident ");
	default:
		goto again;
		break;
	}
}

void writeStringLiteral()
{
	switch (uniform(0, 6))
	{
	case 0: writeDoubleQuoteStringLiteral(); break;
	case 1: writeHeredocStringLiteral(); break;
	case 2: writeDelimitedStringLiteral(); break;
	case 3: writeBacktickStringLiteral(); break;
	case 4: writeRDoubleQuoteStringLiteral(); break;
	case 5: writeCharLiteral(); break;
	default: break;
	}
}

void writeDoubleQuoteStringLiteral()
{
	write('"');
	auto length = uniform(0, 30);
	foreach (i; 0 .. length)
	{
	again:
		auto j = uniform(0, 128);
		switch (j)
		{
		case 0: .. case 6: goto again; break;
		case 7:  write(`\a`); break;
		case 8:  write(`\b`); break;
		case 9:  write(`\t`); break;
		case 10: write(`\n`); break;
		case 11: write(`\v`); break;
		case 12: write(`\f`); break;
		case 13: write(`\r`); break;
		case 14: .. case 31: goto again; break;
		case 32: .. case 33:
		case 35: .. case 91:
		case 93: .. case 176:
			write(cast(char) j);
			break;
		case 34: write(`\"`); break;
		case 92: write(`\\`); break;
		default: goto again; break;
		}
	}
	write('"');
	write([' ', 'c', 'w', 'd'][uniform(0, 4)]);
	write(' ');
}

void writeHeredocStringLiteral()
{

}

void writeDelimitedStringLiteral()
{

}

void writeBacktickStringLiteral()
{

}

void writeRDoubleQuoteStringLiteral()
{

}

void writeCharLiteral()
{

}
