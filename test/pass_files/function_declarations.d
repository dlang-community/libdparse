void foo() {}
void foo() in {} body {}
void foo() in {} out {} body {}
void foo() in {} out(a) {} body {}
void foo() out(a) {} in {} body {}

void foo(T)();
void foo(T)() in {} body {}
void foo(T)() in {} out {} body {}
void foo(T)() in {} out(a) {} body {}
void foo(T)() out(a) {} in {} body {}

void foo(T)() if (something) {}
void foo(T)() if (something) in {} body {}
void foo(T)() if (something) in {} out {} body {}
void foo(T)() if (something) in {} out(a) {} body {}
void foo(T)() if (something) out(a) {} in {} body {}

