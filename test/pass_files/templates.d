template a(b) {}
template a(b, c) {}
template a(b, c) if (d) {}
template a(alias int a : 10) {}
template a(alias int a : int) {}
template a(alias int a : 10) {}
template a(alias int a : int = 10) {}
template a(alias int a : int) {}
template a(alias int a = 10) {}
template a(alias int a = int) {}
template a(alias b) {}
template a(alias b, c : d) {}
template a(alias b, c : int) {}
template a(alias b, c : int[]) {}
template a(b) if (is (a : struct)) {}
template a(b) if (is (a : union)) {}
template a(b) if (is (a : class)) {}
template a(b) if (is (a : interface)) {}
template a(b) if (is (a : enum)) {}
template a(b) if (is (a : function)) {}
template a(b) if (is (a : delegate)) {}
template a(b) if (is (a : super)) {}
template a(b) if (is (a : return)) {}
template a(b) if (is (a : __parameters)) {}
template a(b) if (is (a : const)) {}
template a(b) if (is (a : immutable)) {}
template a(b) if (is (a : inout)) {}
template a(b) if (is (a : shared)) {}
enum A(T) = A;
template A(T) if (is (T u : v, W)) {}
mixin template A(T) { T t; }
size_t replicateBits(size_t , )() {}
