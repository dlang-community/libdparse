@safe struct S {
    static int a;
    int b;
    ref int fun() { return a; } // fine, callers assume infinite lifetime
    ref int gun() { return b; } // ERROR! Cannot return a direct member
    ref int hun() return { return b; } // fine, result is scoped within this
    @safe ref int fun(ref return float x);
}
@safe struct S {
    private int x;
    ref int get() return { return x; } // should work, see next section
}
