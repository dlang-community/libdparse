// things that test that they abort properly, but not emit any AST:

void asm1() {
	asm {
		align true;
	}
}

void asm2() {
	asm const {
	}
}

void randomContinue() {
	continue 4;
}

void randomBreak() {
	break 4;
}

void randomGoto() {
	goto 4;
}

void randomDebug() {
	debug = true;
}

void randomDebug2() {
	debug (true)
	{
	}
}

void foo3()
in(x > 4)

void thisIsCurrentlyEaten() {}

struct 5
{
	void ignoredAsWell() {}
}

void brokenSwitch()
{
	switch (;)
	{
	}
}

void brokenCall()
{
	foo(
}

void randomVersion() {
	version (true)
	{
	}
}

void randomVersion2() {
	version = x("");
}

class X : Sub
if (x)
void ignored() {}
void afterClass() {}

version;

void versionWorks() {}

mixin = 4;

void mixinWorks() {}

version = true;

void discardedVersion() {}

// things that test that they abort properly and emit partial or corrected AST:

void foo() out(x > 4) {}
void foo2() out(true; x > 4) {}

version = 1

void bar()
{
	do {

	} while (x)
}

void baz()
{
	import a
	import b;
}

@