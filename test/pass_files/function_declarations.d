void foo(int a ...) {}
void foo(...) in {} body {}
void foo(double) in {} out {} body {}
void foo(const int) in {} out(a) {} body {}
void foo(lazy real) out(a) {} in {} body {}

void foo(T)();
void foo(T)() in {} body {}
void foo(T)() in {} out {} body {}
void foo(T)() in {} out(a) {} body {}
void foo(T)() out(a) {} in {} body {}

void foo(T)(immutable(T) t) if (something) {}
void foo(T)(in int great) if (something) in {} body {}
void foo(T)(final void* param) if (something) in {} out {} body {}
void foo(T)(char c = 'a') if (something) in {} out(a) {} body {}
void foo(T)(char s[]) if (something) out(a) {} in {} body {}

auto foo(int ...) { return 1; }
auto ref foo() { return 1; }
ref auto foo() { return 1; }
const foo() { return 1; }
auto inout foo() { return 1; }
inout auto foo() { return 1; }

int foo() pure { return 1; }
int foo() const { return 1; }
int foo() inout { return 1; }
int foo() immutable { return 1; }
int foo() shared { return 1; }
int foo() const @safe { return 1; }
int foo() const @safe nothrow { return 1; }

auto a = function int (int a) { return a * 2; };
auto a = function int (int a) pure { return a * 2; };
auto a = function int (int a) @whatever { return a * 2; };
auto a = function (int a) => a * 2;
auto a = (int a) => a * 2;
auto a = function int (int a) => a * 2;
void bar()
{
	doStuff(function int(int a) { return a / 2; });
	doStuff(function int(int a) body { return a / 2; });
	doStuff(function int(int a) in { assert (a > 10); } body { return a / 2; });
}

void cVarArg(int, ...);
enum bool isInputRange = is(typeof((inout int = 0){}));
auto a = b => b * 2;

int typesafeVarArg(int a = 1, int[] rest = [] ...) { return 1; }
