union { int a; char b; }
union SomeUnion;
union SomeUnion { int a; int b; }
union SomeUnion(T) { int a; int b; }
union SomeUnion(T) if (z) { int a; int b; }

