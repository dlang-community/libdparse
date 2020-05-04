module hasasm;

void doStuff()
{
	asm nothrow @safe @nogc
	{
		xor RAX, RAX;
	}
	asm
	{
		mov EAX, 10u;
		mov RAX, 10UL;
		mov RAX, a;
		ret;
		mov RAX, a[100];
		mov RAX, [a + 100];
		mov RAX, a ? b : c;
		align 100;
		align whatever;
	label:
		mov RAX, RCX;
		db "test";
		mov RAX, 100;
		mov RAX, 10.0f;
		mov RAX, 10.0;
		mov ST(0), 1;
		add near ptr [EAX], 3;
		add byte ptr [EAX], 3;
		mov RAX, a.b.c;
		mov RAX, ~a;
		mov RAX, !a;
		mov RAX, -a;
		mov RAX, +a;
		mov RAX, offsetof a;
		mov EAX, FS:4;
		mov EAX, FS:CL;
		push dword ptr FS:[0];
		jge short L_largepositive;
		lea EDX,[ECX][ECX*8];
		in AL,6;
		out AL,6;
		int 3;
	}
	asm
	{
		align 4	;
		LABEL:	;
		;

		mov EAX, this;
		mov ECX, __LOCAL_SIZE;
	}
}
