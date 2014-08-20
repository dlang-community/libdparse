#!/usr/bin/rdmd

/// single line comment
/* StarSlash */
/***** ******/
/**
 * 123
 */
/+ StarPlus +/
/++
 + 456
 +/
/+ StarPlus /+ Nested +/ +/

#line 10 "whatever.d"

auto a = 1u;
auto a = 1U;
auto a = 1uL;
auto a = 1UL;
auto a = 1L;
auto a = 1Lu;
auto a = 1LU;
auto a = 1i;
auto a = 1.0;
auto a = 1.0L;
auto a = 1e10;
auto a = 1.e10;
auto a = 1e1L;
auto a = 1e1f;
auto a = 1e1F;
auto a = 1e1i;
auto a = 1e+1;
auto a = 1e+_1;
auto a = 1e-1;
auto a = 1e-_1;
auto a = b[1..2];
auto a = 0x1p10;
auto a = 0x1_2u;
auto a = 0x1_2.fi;
auto a = 0x1_2L;
auto a = 0b1_0;
auto a = 0b1u;
auto a = 0b1L;
auto a = 0b1UL;
auto a = 0b1U;
auto a = 0b1LU;
string a = .1;
string a = 'a';
string a = '\0';
string a = '\01';
string a = '\012';
string a = '\123';
string a = '\u12aA';
string a = '\U12aA5678';
string a = "\"\?\\\0\a\b\f\n\r\t\v\xff\1\u1234\UaA345678";
string a = "str"c;
string a = "str"d;
string a = "str"w;
string a = `str`;
string a = `str''""`;
string a = r"\a\b\c"c;
string a = x"12 34 56 78 90 ab cd ef AB CD EF";
string a = x"AB"c;
string a = x"AB"d;
string a = x"AB"w;
string a = q{int a;};
string a = q{{int a;}};
string a = q{int a;}c;
string a = q{int a;}d;
string a = q{int a;}w;
string a = q"( " )";
string a = q"( (") )";
string a = q"< " >";
string a = q"< <"> >";
string a = q"{ " }";
string a = q"{ {"} }";
string a = q"[ " ]";
string a = q"[ ["] ]";
string a = q"IDENTIFIER
"
IDENTIFIER";

