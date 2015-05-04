/// Ditto
template octal(alias s)
    if (isIntegral!(typeof(s)))
{
    enum auto octal = octal!(typeof(s), to!string(s));
}
