class A;
class B {}
class C : B {}
class D(T) if (Z) : B {}
class E(T) : B if (Z) {}
class F(T);
class G(T) if (Z);
class H : public A {}
class H : typeof(A).B {}
