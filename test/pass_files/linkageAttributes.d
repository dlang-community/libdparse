private:
extern(C) int a;
extern(D) int b;
extern(Windows) int c;
extern(Pascal) int d;
extern(System) int e;
extern(Objective-C) int f ;
extern(C++) int g;
extern(C++, a.b.c) int h;
extern(C++, struct) int i;
extern(C++, class) int j;

extern(C++, "abc") int k;
extern(C++, "a", "b", "c") int k;
extern(C++, (abc)) int m;
extern(C++, ("abc")) int n;
