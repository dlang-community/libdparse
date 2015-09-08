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
