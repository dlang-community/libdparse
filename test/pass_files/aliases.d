alias a = b;
alias b a;
alias int b;
alias int b, c, d, e;
alias c = int;
alias x = y, w = z;
alias const immutable int constInt;
alias shared(int) sharedInt;
alias a(b) = c;
alias a(b) = c, d = e, f = g;
alias a(b) = const c, d = int;
alias a(b) = const c, d = double[int[string]];
alias a = c.d.e;
struct S
{
	int a;
	alias a this;
}
