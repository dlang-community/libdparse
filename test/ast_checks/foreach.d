void foo(T)(T[] arr)
{
	foreach (enum ref scope const inout alias f; arr) {}
}
