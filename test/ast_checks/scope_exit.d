void foo() {
	scope (exit)
		int hi;
}

void bar() {
	// wtf does this even mean, why does this work with DMD?!
	int x = 1;
	switch (x)
	{
		case 1:
			break;
		case 2:
			break;
			scope (exit)
				default:
					foo();
			break;
	}
}