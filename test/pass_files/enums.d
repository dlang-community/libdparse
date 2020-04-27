enum A { a }
enum B { a, b, c }
enum C { a = 10, b = 20, d = int.init, e }
enum D : int { a }
enum : int { a }
enum { int b = 100 }
enum
{
    /// doc for a
    a,
    b, // doc for b
    c // doc for c
}

enum E
{
    @disable member,
    @A @B deprecated("meep") member,
    deprecated("meep") member
}

// https://github.com/dlang-community/libdparse/issues/390
enum F;
enum G : long;
