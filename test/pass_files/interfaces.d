interface A;
interface B {}
interface C : B {}
interface D(T) if (Z) : B {}
interface E(T) : B if (Z) {}
interface F(T);
interface G(T) if (Z);
