import std : writeln, format;

int get()
{
    return 1;
}

void main()
{
    switch (auto x = get()) {
        default:
            writeln("inside switch: ", x);
    }
}
