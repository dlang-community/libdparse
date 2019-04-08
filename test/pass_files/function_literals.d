void main()
{
	auto foo1() pure immutable
	{
		return 0;
	}

	auto foo2() pure const
	{
		return 0;
	}
}

void main()
{
	auto dg1 = () pure immutable
	{
		return 0;
	};
	auto dg2 = () pure const
	{
		return 0;
	};
}

void testRefReturns()
{
	alias flit11 = function ref int () { return a; };
	alias flit12 = delegate ref int () { return a; };
	alias flit13 = ref () {return a; };

	auto aflit11 = function ref int () { return a; };
	auto aflit12 = delegate ref int () { return a; };
	auto aflit13 = ref () {return a; };

	alias flit21 = function ref int () => a;
	alias flit22 = delegate ref int () => a;
	alias flit23 = ref () => a;

	auto aflit21 = function ref int () => a;
	auto aflit22 = delegate ref int () => a;
	auto aflit23 = ref () => a;

	(ref () => x )() = 1;
	assert((funa16271!(         ref    (ref a) => a)(x) += 1) == 7 );
	assert((funa16271!(function ref    (ref a) => a)(x) += 1) == 8 );
	assert((funa16271!(function ref int(ref a) => a)(x) += 1) == 9 );
	assert((funa16271!(delegate ref    (ref a) => a)(x) += 1) == 10);
	assert((funa16271!(delegate ref int(ref a) => a)(x) += 1) == 11);
}
