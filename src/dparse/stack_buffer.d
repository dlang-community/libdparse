module dparse.stack_buffer;

import std.traits;

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

		if (arr.ptr != stackSpace.ptr)
		{
			if (_length + T.sizeof > arr.length)
			{
				size_t nl = arr.length << 1;
				while (_length + T.sizeof < nl)
					nl <<= 1;
				arr = (cast(ubyte*) realloc(arr.ptr, nl))[0 .. nl];
			}
		}
		else if (_length + T.sizeof > stackSpace.length)
		{
			size_t nl = arr.length << 1;
			while (_length + T.sizeof < nl)
				nl <<= 1;
			arr = (cast(ubyte*) malloc(nl))[0 .. nl];
		}
		static if (is(T == class) || isPointer!T)
			arr[_length .. _length + T.sizeof] = (cast(ubyte*) t)[0 .. T.sizeof];
		else
			arr[_length .. _length + T.sizeof] = (cast(ubyte*) &t)[0 .. T.sizeof];
		_length += T.sizeof;
		return true;
	}

	~this()
	{
		import core.stdc.stdlib : free;

		if (arr.ptr != stackSpace.ptr)
			free(arr.ptr);
		arr[] = 0;
	}

	auto opSlice()
	{
		return arr[0 .. length];
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

	align(8) ubyte[1024] stackSpace;
	ubyte[] arr;
	uint _length;
}

import std.stdio;
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
