class A;
class B {}
class C : B {}
class C : TList[0] {}
class C : TList[0].TList[0].Prop {}
class D(T) if (Z) : B {}
class E(T) : B if (Z) {}
class F(T);
class G(T) if (Z);
class H : public A {}
class H : typeof(A).B {}
class I { int x; alias y = this.x; }
class J : K { int x; alias y = super.x; }
