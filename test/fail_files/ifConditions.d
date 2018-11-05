void foo()
{
    if (auto const(Type)* data = call()){}
    if (const const a = call()){}
    if (auto auto a = call()){}
    if (Type!(0) = expr()){}
    if (Type!(0) i){}
}
