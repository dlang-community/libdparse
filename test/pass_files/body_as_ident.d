void body()
in 
{
} 
body
{ 
    Corpulence body; 
    alias b = body;
}

struct Foo(Body)
{
    static if (is(Body)){}
}

enum Body;

@Body void foo();
