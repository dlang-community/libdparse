void a()
{
	switch (auto line = readln)
	{
		default:
			line.strip;
	}
}

void b()
{
	switch (scope line = readln)
	{
		default:
			line.strip;
	}
}

void c()
{
	switch (const line = readln)
	{
		default:
			line.strip;
	}
}

void d()
{
	switch (const inout string line = readln)
	{
		default:
			line.strip;
	}
}