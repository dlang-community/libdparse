int main()
{
    foo(throw someThrowable, bar);

    return foo ? bar : throw new Exception("Hello, World!");
}
