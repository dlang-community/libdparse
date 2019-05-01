alias a = b;
alias b a;
alias int b;
alias int b, c, d, e;
alias c = int;
alias x = y, w = z;
alias const immutable int constInt;
alias shared(int) sharedInt;
alias a(b) = c;
alias a(b) = c, d = e, f = g;
alias a(b) = const c, d = int;
alias a(b) = const c, d = double[int[string]];
alias a = c.d.e;
struct S
{
	int a;
	alias a this;
}
class Foo
{
	void bar()
	{
		alias self = this;
	}
}

// C-style aliases
alias int GetterType() @property;
alias int SetterType(int) @property;
alias string SetterType(int, string) @nogc;
alias string SetterType(int, string) @safe;
alias int F1();
alias @property int F2();
alias string F3();
alias nothrow @trusted uint F4();
alias int F5(Object);
alias bool F6(Object);
alias int F1();
alias int F2() pure nothrow;
alias int F3() @safe;
alias int F23() @safe pure nothrow;

// return type covariance
alias long F4();
class C {}
class D : C {}
alias C F5();
alias D F6();
alias typeof(null) F7();
alias int[] F8();
alias int* F9();

// variadic type equality
alias int F10(int);
alias int F11(int...);
alias int F12(int, ...);

// linkage equality
alias extern(C) int F13(int);
alias extern(D) int F14(int);
alias extern(Windows) int F15(int);

// ref & @property equality
alias int F16(int);
alias ref int F17(int);
alias @property int F18(int);

// function types with '='
alias Fun1(T) = T(T t) @safe;
alias Fun2 = void(int,int,int) pure nothrow;
alias Fun3 = const void(int,int,int) @trusted;
alias Fun4(T...) = shared const(Foo!Bar)(T t) const;
