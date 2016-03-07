module dparse.rollback_allocator;

//version = debug_rollback_allocator;

/**
 * Pointer-bump allocator with rollback functionality.
 */
struct RollbackAllocator
{
public:

    @disable this(this);

    ~this()
    {
        while (first !is null)
            deallocateNode();
    }

    /**
     * Allocates `size` bytes of memory.
     */
    void[] allocate(size_t size)
    out (arr)
    {
        assert(arr.length == size);
    }
    body
    {
        if (first is null || first.used + size > first.mem.length)
            allocateNode(size);
        immutable fu = first.used;
        immutable end = fu + size;
        void[] m = first.mem[fu .. end];
        first.used = end;
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
        while (!first.contains(point))
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
        assert(first.used <= first.mem.length);
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
        import std.algorithm : max;
        import std.experimental.allocator.mallocator : Mallocator;
        import std.conv : emplace;

        enum ALLOC_SIZE = 1024 * 8;

        ubyte[] m = cast(ubyte[]) Mallocator.instance.allocate(max(size + Node.sizeof, ALLOC_SIZE));
        version (debug_rollback_allocator)
            m[] = 0;
        Node* n = emplace!Node(cast(Node*) m.ptr, first, 0, m[Node.sizeof .. $]);
        assert((cast(size_t) n.mem.ptr) % 8 == 0, "The memoriez!");
        first = n;
    }

    void deallocateNode()
    {
        assert(first !is null);
        import std.experimental.allocator.mallocator : Mallocator;

        Node* next = first.next;
        ubyte[] mem = (cast(ubyte*) first)[0 .. Node.sizeof + first.mem.length];
        version (debug_rollback_allocator)
            mem[] = 0;
        Mallocator.instance.deallocate(mem);
        first = next;
    }

    Node* first;
}
