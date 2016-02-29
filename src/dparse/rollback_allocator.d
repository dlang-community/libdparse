module dparse.rollback_allocator;

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
            // Align to 8 bytes
            //if (end & 0b111)
                //first.used = (end & ~0b111) + 0b1000;
            //else
                first.used = end;
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

    enum ALLOC_SIZE = 1024 * 1024 * 4;

    static struct Node
    {
        Node* next;
        size_t used;
        void[] mem;

        bool contains(size_t p) pure nothrow @nogc @safe
        {
            return p > cast(size_t) mem.ptr && p < cast(size_t) mem.ptr + mem.length;
        }
    }

    void allocateNode()
    {
        import std.experimental.allocator.mallocator : Mallocator;

        void[] m = Mallocator.instance.allocate(ALLOC_SIZE);
        Node* n = cast(Node*) m.ptr;
        n.next = first;
        n.used = 0;
        pragma(msg, Node.sizeof);
        n.mem = m[Node.sizeof .. $];

        first = n;
    }

    void deallocateNode()
    {
        import std.experimental.allocator.mallocator : Mallocator;
        import std.experimental.allocator : dispose;

        Node* next = first.next;
        void[] mem = (cast(void*) first)[0 .. ALLOC_SIZE];
        (cast(ubyte[]) mem)[] = 0;
        Mallocator.instance.deallocate(mem);
        first = next;
    }

    Node* first;
}
