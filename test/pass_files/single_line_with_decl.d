module a;

enum A{a0}

void foo()
{
    with(A) const i = 0;
    with(A)
    {
        const i = 0;
    }
}
