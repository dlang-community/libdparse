void foo()
{
    if (const(Type)* data = call()){}
    if (const a = call()){}
    if (const shared a = call()){}
    if (auto a = call()){}
    if (immutable shared(Type) a = call()){}
    if (a) {}
    if (T t = T.init) {}
    if (T!0 t = T.init) {}
    if (true) {}
}

void main()
{
    if ((a && b) || c) {}
    if (a && b || c) {}
}
