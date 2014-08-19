void foo()
{
    try {
        doSomething();
    } catch {
        doSomethingElse();
    }

    try {
        doSomething();
    } catch (Exception) {
        doSomethingElse();
    }

    try {
        doSomething();
    } catch (Exception e) {
        doSomethingElse();
    }

	try
		doSomething();
	finally
		doSomethingElse();
}
