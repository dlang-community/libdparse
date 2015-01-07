deprecated("this code is older than the dinosaurs") module declarations;
int a;
int[] a;
int[string] a;
int a, b;
int a = .b.c;
a.b c = d;
.a.b c = d;
typeof(a) c = d;
typeof(a).b c = d;
align int a;
align(8) int a;
align(8) align int a;
int[] a = [];
auto a = [1, 2, 3];
auto a = [a:1, b:2, c:3];
auto a = b, c = d;
static if (true)
	int a;
else
	int b;

debug void foo();
debug(something) void foo();
debug(100) void foo();
debug = 101;
debug = identifier;

version(AArch64) enum x = 100;
version = coverage;

static if (true):
mixin ("int a;");
mixin something;
mixin something!A;
mixin duff!(i, j, delegate { foo13(i); });
mixin typeof(something!A).x;
template mix(){
	int x;
}
mixin .mix;
__vector(int[4]) intVector;
;
{
	int a;
}
enum a = 1;
SomeStruct a = { a : 10, b : 20 };
int[a .. b] c;
int function(int) a;
int function(int) const a;
int delegate(int) a;
int a = typeid(int).alignof;
int a = typeid(10).alignof;
int a = (int).sizeof;
enum string STRING_CONSTANT = "abc";
Size[][] minSizes = new Size[][](cols, rows);
