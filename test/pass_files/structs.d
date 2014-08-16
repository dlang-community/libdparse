struct S
{
	@disable this(this);
	this(this) { this.y++; }
	this(this) @whatever { this.y++; }
	invariant()
	{
		assert (x == 10);
	}
}
