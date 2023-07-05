mixin("int") variableName;
void foo(mixin("int") arg) {
	mixin("int") localVar;
}
struct S {
	mixin("int") foo;
}
