alias aaa = a => a * 2;
alias bbb = (a) => a * 2;
alias ccc = (a) nothrow => a * 2;
alias ddd = function void(a) => a * 2;
alias eee = function void(a) nothrow => a * 2;
alias fff = function void(a) { return 100; };
alias ggg = function void(a) in { } out { } body { return 100; };
alias hhh = delegate void(a) => a * 2;
alias iii = delegate void(a) nothrow => a * 2;
alias jjj = delegate void(a) { return 100; };
alias kkk = delegate void(a) in { } out { } body { return 100; };
alias lll = { return 100; };
alias mmm = () { return 100; };
alias nnn = () @nogc { return 100; };
alias fun = (@(1) inout @(1) a) => {};

// The following is valid according to D's grammar, but rejected by dmd
/+unittest
{
	alias abc = in { } out { } body { return 100; };
	alias abc = () in{ } out { } body { return 100; };
	alias abc = () @nogc in{ } out { } body { return 100; };
	doThings(in { } out { } body { return 100; });
	auto a = in { } out { } body { return 100; };
}
+/
