module issue0291;

void foo()
in { } // "so you need do{}?"
out (; true) // No you don't, but libdparse thinks you still do.
{ }
