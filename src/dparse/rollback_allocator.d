module dparse.rollback_allocator;

import core.memory : GC;

//version = debug_rollback_allocator;

/**
 * Pointer-bump allocator with rollback functionality.
 */
struct RollbackAllocator
{
public:

    // must be multiple of 8
    enum memoryAlignment = 16u;

    @disable this(this);

    ~this()
    {
        while (first !is null)
            deallocateNode();
    }

    /**
     * Allocates `size` bytes of memory.
     */
    void[] allocate(const size_t size)
    out (arr)
    {
        assert(arr.length == size);
    }
    do
    {
        import std.algorithm.comparison : min;

        if (first is null)
            allocateNode(size);

        // Memory align the size
        immutable size_t s = size & ~(cast(size_t) memoryAlignment - 1);
        immutable size_t s2 = s == size ? size : s + memoryAlignment;

        size_t fu = first.used;
        size_t end = fu + s2;
        //assert(end >= fu + size);
        //assert(end % 8 == 0);
        if (end > first.mem.length)
        {
            allocateNode(size);
            fu = first.used;
            end = fu + s2;
        }
        //assert((cast(size_t) first.mem.ptr) % 8 == 0);
        //assert(((cast(size_t) first.mem.ptr) + end) % 8 == 0);
        void[] m = first.mem[fu .. fu + size];
        // alignment can make our size here bigger than what we actually have, so we clamp down to the used amount
        first.used = min(end, first.mem.length);
        return m;
    }

    /**
     * Rolls back the allocator to the given checkpoint.
     */
    void rollback(size_t point)
    {
        import std.stdio : stderr;

        if (point == 0)
        {
            while (first)
                deallocateNode();
            return;
        }
        else
            assert(contains(point), "Attepmted to roll back to a point not in the allocator.");

        // while `first !is null` is always going to pass after the contains(point) check, it may no longer pass after deallocateNode
        while (first !is null && !first.contains(point))
            deallocateNode();
        assert(first !is null);

        immutable begin = point - cast(size_t) first.mem.ptr;
        version (debug_rollback_allocator)
            (cast(ubyte[]) first.mem)[begin .. $] = 0;
        first.used = begin;
        assert(cast(size_t) first.mem.ptr + first.used == point);
    }

    /**
     * Get a checkpoint for the allocator.
     */
    size_t setCheckpoint() const nothrow @nogc
    {
        assert(first is null || first.used <= first.mem.length);
        return first is null ? 0 : cast(size_t) first.mem.ptr + first.used;
    }

    /**
     * Allocates a T and returns a pointer to it
     */
    auto make(T, Args...)(auto ref Args args)
    {
        import std.algorithm.comparison : max;
        import std.experimental.allocator : stateSize;
        import std.conv : emplace;

        void[] mem = allocate(max(stateSize!T, 1));
        if (mem.ptr is null)
            return null;
        static if (is(T == class))
            return emplace!T(mem, args);
        else
            return emplace(cast(T*) mem.ptr, args);
    }

private:

    // Used for debugging
    bool contains(size_t point) const
    {
        for (const(Node)* n = first; n !is null; n = n.next)
            if (n.contains(point))
                return true;
        return false;
    }

    static struct Node
    {
        Node* next;
        size_t used;
        ubyte[] mem;

        bool contains(size_t p) const pure nothrow @nogc @safe
        {
            return p >= cast(size_t) mem.ptr && p <= cast(size_t) mem.ptr + mem.length;
        }
    }

    void allocateNode(size_t size)
    {
        import core.exception : onOutOfMemoryError;
        import std.algorithm : max;
        import std.conv : emplace;
        import std.experimental.allocator.mallocator : AlignedMallocator;

        enum ALLOC_SIZE = 1024 * 8;

        ubyte[] m = cast(ubyte[]) AlignedMallocator.instance.alignedAllocate(max(size + Node.sizeof, ALLOC_SIZE), memoryAlignment);
        if (m is null)
            onOutOfMemoryError();
        GC.addRange(m.ptr, m.length);

        version (debug_rollback_allocator)
            m[] = 0;
        Node* n = emplace!Node(cast(Node*) m.ptr, first, 0, m[Node.sizeof .. $]);
        assert((cast(size_t) n.mem.ptr) % 8 == 0, "The memoriez!");
        first = n;
    }

    void deallocateNode()
    {
        assert(first !is null);
        import std.experimental.allocator.mallocator : AlignedMallocator;

        Node* next = first.next;
        ubyte[] mem = (cast(ubyte*) first)[0 .. Node.sizeof + first.mem.length];
        version (debug_rollback_allocator)
            mem[] = 0;
        GC.removeRange(mem.ptr);
        AlignedMallocator.instance.deallocate(mem);
        first = next;
    }

    Node* first;
}

@("most simple usage, including memory across multiple pointers")
unittest
{
    RollbackAllocator rba;
    size_t[10] checkpoint;
    foreach (i; 0 .. 10)
    {
        checkpoint[i] = rba.setCheckpoint();
        rba.allocate(4000);
    }

    foreach_reverse (i; 0 .. 10)
    {
        rba.rollback(checkpoint[i]);
    }
}

@("many allocates and frees while leaking memory")
unittest
{
    RollbackAllocator rba;
    foreach (i; 0 .. 10)
    {
        size_t[3] checkpoint;
        foreach (n; 0 .. 3)
        {
            checkpoint[n] = rba.setCheckpoint();
            rba.allocate(4000);
        }
        foreach_reverse (n; 1 .. 3)
        {
            rba.rollback(checkpoint[n]);
        }
    }
}

@("allocating overly big")
unittest
{
    import std.stdio : stderr;

    RollbackAllocator rba;
    size_t[200] checkpoint;
    size_t cp;
    foreach (i; 1024 * 8 - 100 .. 1024 * 8 + 100)
    {
        try
        {
            checkpoint[cp++] = rba.setCheckpoint();
            rba.allocate(i);
        }
        catch (Error e)
        {
            stderr.writeln("Unittest: crashed in allocating ", i, " bytes");
            throw e;
        }
    }

    foreach_reverse (i, c; checkpoint[0 .. cp])
    {
        try
        {
            rba.rollback(c);
        }
        catch (Error e)
        {
            stderr.writeln("Unittest: crashed in rolling back ", i, " (address ", c, ")");
            throw e;
        }
    }
}
