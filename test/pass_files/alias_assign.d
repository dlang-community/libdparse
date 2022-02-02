template T(X)
{
     alias A = int;
     A = X;
     struct B
     {
          A t;
     }
}

void fun()
{
    auto b = T!float.B();
}

// https://github.com/dlang/phobos/pull/8033
template EraseAll(args...)
if (args.length >= 1)
{
    alias EraseAll = AliasSeq!();
    static foreach (arg; args[1 .. $])
        static if (!isSame!(args[0], arg))
            EraseAll = AliasSeq!(EraseAll, arg);
}

// https://github.com/dlang/phobos/pull/8034
template NoDuplicates(args...)
{
    alias NoDuplicates = AliasSeq!();
    static foreach (arg; args)
        NoDuplicates = AppendUnique!(NoDuplicates, arg);
}

// https://github.com/dlang/phobos/pull/8036
template ReplaceAll(args...)
{
    alias ReplaceAll = AliasSeq!();
    static foreach (arg; args[2 .. $])
    {
        static if (isSame!(args[0], arg))
            ReplaceAll = AliasSeq!(ReplaceAll, args[1]);
        else
            ReplaceAll = AliasSeq!(ReplaceAll, arg);
    }
}

// https://github.com/dlang/phobos/pull/8037
template Reverse(args...)
{
    alias Reverse = AliasSeq!();
    static foreach_reverse (arg; args)
        Reverse = AliasSeq!(Reverse, arg);
}

// https://github.com/dlang/phobos/pull/8038
template MostDerived(T, TList...)
{
    import std.traits : Select;
    alias MostDerived = T;
    static foreach (U; TList)
        MostDerived = Select!(is(U : MostDerived), U, MostDerived);
}

// https://github.com/dlang/phobos/pull/8044
template Repeat(size_t n, items...)
{
    static if (n == 0)
    {
        alias Repeat = AliasSeq!();
    }
    else
    {
        alias Repeat = items;
        enum log2n =
        {
            uint result = 0;
            auto x = n;
            while (x >>= 1)
                ++result;
            return result;
        }();
        static foreach (i; 0 .. log2n)
        {
            Repeat = AliasSeq!(Repeat, Repeat);
        }
        Repeat = AliasSeq!(Repeat, Repeat!(n - (1u << log2n), items));
    }
}

// https://github.com/dlang/phobos/pull/8047
template Stride(int stepSize, Args...)
if (stepSize != 0)
{
    alias Stride = AliasSeq!();
    static if (stepSize > 0)
    {
        static foreach (i; 0 .. (Args.length + stepSize - 1) / stepSize)
            Stride = AliasSeq!(Stride, Args[i * stepSize]);
    }
    else
    {
        static foreach (i; 0 .. (Args.length - stepSize - 1) / -stepSize)
            Stride = AliasSeq!(Stride, Args[$ - 1 + i * stepSize]);
    }
}
