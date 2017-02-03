void foo()
{
    if (auto const(Type)* data = call()){}
    if (const const a = call()){}
    if (auto auto a = call()){}
}