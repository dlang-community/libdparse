module dparse.stack_buffer;

import std.traits;

//version = debug_stack_allocator;

struct StackBuffer
{
    bool put(T)(T t)
    {
        import stdx.allocator.mallocator : Mallocator;

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
                size_t newLength = arr.length << 1;
                while (_length + T.sizeof > newLength)
                    newLength <<= 1;
                Mallocator.instance.reallocate(arr, newLength);
                version (debug_stack_allocator)
                    (cast(ubyte[]) arr)[_length .. $] = 0;
            }
        }
        else if (_length + T.sizeof > stackSpace.length)
        {
            size_t newLength = stackSpace.length << 1;
            while (_length + T.sizeof > newLength)
                newLength <<= 1;
            arr = Mallocator.instance.allocate(newLength);
            version (debug_stack_allocator)
                (cast(ubyte[]) arr)[] = 0;
            arr[0 .. stackSpace.length] = stackSpace[];
        }
        arr[_length .. _length + T.sizeof] = (cast(void*) &t)[0 .. T.sizeof];
        _length += T.sizeof;
        return true;
    }

    ~this()
    {
        import stdx.allocator.mallocator:Mallocator;

        version (debug_stack_allocator)
            (cast(ubyte[]) arr)[] = 0;
        if (arr.ptr !is stackSpace.ptr)
            Mallocator.instance.deallocate(arr);
    }

    void[] opSlice()
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

    void[8 * 16] stackSpace;
    void[] arr;
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
        void[100] u;
    }

    S s;
    sb.put(s);

    class B
    {
        int b;
    }

    class D : B
    {
        double d;
    }

    B b = new B;
    sb.put(b);

    B d = new D;
    sb.put(d);
}
