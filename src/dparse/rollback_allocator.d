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
        bool newNode = first is null;
        if (newNode)
            allocateNode();
    alloc:
        immutable fu = first.used;
        if (fu + size < first.mem.length)
        {
            immutable end = fu + size;
            void[] m = first.mem[fu .. end];
            //Align to 8 bytes
            if (end & 0b111)
                first.used = (end & ~0b111L) + 0b1000L;
            else
                first.used = end;
            assert(first.used > fu);
            return m;
        }
        if (newNode)
            return [];
        else
        {
            allocateNode();
            newNode = true;
            goto alloc;
        }
    }

    /**
     * Rolls back the allocator to the given checkpoint.
     */
    void rollback(size_t point)
    {
        while (first !is null && !first.contains(point))
            deallocateNode();
        if (first !is null)
        {
            immutable begin = point - cast(size_t) first.mem.ptr;
            version (debug_rollback_allocator)
                (cast(ubyte[]) first.mem)[begin .. $] = 0;
            first.used = begin;
        }
    }

    /**
     * Get a checkpoint for the allocator.
     */
    size_t setCheckpoint()
    {
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

    enum ALLOC_SIZE = 1024 * 4;

    static struct Node
    {
        Node* next;
        size_t used;
        ubyte[] mem;

        bool contains(size_t p) pure nothrow @nogc @safe
        {
            return p >= cast(size_t) mem.ptr && p < cast(size_t) mem.ptr + mem.length;
        }
    }

    void allocateNode()
    {
        import std.experimental.allocator.mallocator : Mallocator;
        import std.conv : emplace;

        ubyte[] m = cast(ubyte[]) Mallocator.instance.allocate(ALLOC_SIZE);
        version (debug_rollback_allocator)
            m[] = 0;
        Node* n = emplace!Node(cast(Node*) m.ptr);
        n.next = first;
        n.used = 0;
        n.mem = m[Node.sizeof .. $];
        assert((cast(size_t) n.mem.ptr) % 8 == 0, "The memoriez!");
        first = n;
    }

    void deallocateNode()
    {
        import std.experimental.allocator.mallocator : Mallocator;
        Node* next = first.next;
        ubyte[] mem = (cast(ubyte*) first)[0 .. ALLOC_SIZE];
        version (debug_rollback_allocator)
            mem[] = 0;
        Mallocator.instance.deallocate(mem);
        first = next;
    }

    Node* first;
}
