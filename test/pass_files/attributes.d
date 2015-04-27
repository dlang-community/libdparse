extern(System) int a;
extern(C++) int a;
extern(C++, a) int a;
extern(C++, a.b.c) int a;
extern int a;
@uda int a;
@("uda") int a;
@uda(42) int a;
@uda() int a;
private int a;
public int a;
protected int a;
shared int a;
export int a;
pragma(whatever) int a;
@uda:
public:
int a;
deprecated double q;
deprecated("NEVAR USE THIS") double q;
