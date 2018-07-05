struct S
{
	@disable this(this);
	this() { this.y++; }
	this(int a) { this.y++; }
	this() const { this.y++; }
	this(T)() { this.y++; }
	this(T)() if (U) { this.y++; }
	this(this) { this.y++; }
	this(this) @whatever { this.y++; }
	~this();
	~this() const {}
	invariant()
	{
		assert (x == 10);
	}
	invariant(true);
	invariant(true, "false");
}
struct S;
struct S {}
struct S(T) {}
struct S(T) if (U) {}
