package(std)
ref intersect()
{
}

package(std)
auto intersect()
{
}

package(std)
const intersect()
{
}
public static immutable ctRegex(alias pattern, alias flags=[]) = ctRegexImpl!(pattern, flags).nr;
struct S(T) { static T t = 0; }
immutable a(A) = S!A.t, b(B) = S!B.t;
const c(C) = S!C.t, d = S!int.t;
