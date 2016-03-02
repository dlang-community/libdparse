module dparse.stack_buffer;

import std.traits;

// version = debug_stack_allocator)

struct StackBuffer
{
    bool put(T)(T t)
    {
        import core.stdc.stdlib : malloc, realloc;

        static if (is(T == class) || isPointer!T)
            if (t is null)
                return false;

        if (_length == 0)
            arr = stackSpace[];

        static if (is(T == class))
            static assert(T.sizeof == size_t.sizeof);

        if (arr.ptr != stackSpace.ptr)
        {
            if (_length + T.sizeof > arr.length)
            {
                size_t nl = arr.length << 1;
                while (_length + T.sizeof > nl)
                    nl <<= 1;
                arr = (cast(ubyte*) realloc(arr.ptr, nl))[0 .. nl];
                version (debug_stack_allocator)
                    arr[_length .. $] = 0;
            }
        }
        else if (_length + T.sizeof > stackSpace.length)
        {
            size_t nl = stackSpace.length << 1;
            while (_length + T.sizeof > nl)
                nl <<= 1;
            arr = (cast(ubyte*) malloc(nl))[0 .. nl];
            version (debug_stack_allocator)
                arr[] = 0;
            arr[0 .. stackSpace.length] = stackSpace[];
        }
        arr[_length .. _length + T.sizeof] = (cast(ubyte*) &t)[0 .. T.sizeof];
        _length += T.sizeof;
        return true;
    }

    ~this()
    {
        import core.stdc.stdlib : free;

        if (arr.ptr !is stackSpace.ptr)
            free(arr.ptr);
    }

    ubyte[] opSlice()
    {
        return arr[0 .. _length];
    }

    @disable this(this);

    uint length() const pure nothrow @nogc @safe @property
    {
        return _length;
    }

    alias opDollar = length;

    auto opIndex(size_t i)
    {
        return arr[i];
    }

private:

    ubyte[8 * 16] stackSpace;
    ubyte[] arr;
    uint _length;
}

unittest
{
    StackBuffer sb;
    ubyte[80] u;
    sb.put(u);
    assert(sb.length == 80);
    static struct S
    {
        ubyte[100] u;
    }

    S s;
    sb.put(s);
    writeln(sb.length);
}
