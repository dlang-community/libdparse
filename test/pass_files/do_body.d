void foo(int x)
in
{
    assert(x > 0);
}
do
{

}

int foo2(int x)
in
{
    assert(x > 0);
}
out(r)
{
    assert(r > 0);
}
do
{
    return 0;
}
