class Foo
{
    class Bar
    {
        class Foobar { void method() {} }
    }

    public Bar bar;
}

void main ()
{
    auto foo = new Foo;
    Foo.Bar bar = foo.new Bar;
    auto foo_bar = foo.new Bar;
    auto foobar = foo.bar.new Foobar;
    auto foobar = (foo.bar).new Foobar;
    (foo.new Bar).method;
}
