static this() {}
shared static this() {}
static ~this() {}
shared static ~this() {}

static this() @system {}
shared static this() @system {}
static ~this() @system {}
shared static ~this() @system {}

static this();
shared static this();
static ~this();
shared static ~this();

static this() @system;
shared static this() @system;
static ~this() @system;
shared static ~this() @system;

struct SomeStruct
{
    @disable this();
    shared this() {}
    this() {}
    this(int a) {}
    ~this() {}
    shared ~this() {}
}
