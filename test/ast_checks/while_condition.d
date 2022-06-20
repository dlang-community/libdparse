void a()
{
	while (auto line = readln)
	{
		line.strip;
	}
}

void b()
{
	while (scope line = readln)
	{
		line.strip;
	}
}

void c()
{
	while (const line = readln)
	{
		line.strip;
	}
}

void d()
{
	while (const inout string line = readln)
	{
		line.strip;
	}
}