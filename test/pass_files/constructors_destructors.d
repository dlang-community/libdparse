static this() {}
shared static this() {}
static ~this() {}
shared static ~this() {}

struct SomeStruct
{
    @disable this();
    shared this() {}
    this() {}
    this(int a) {}
    ~this() {}
    shared ~this() {}
}
