import std : writeln, format;

void main()
{
    int i = 10;
    // Silly example I know
    while (int not_four = (i != 4)) {
        writeln(format("%d is not 4!", i));
        i--;
    }
}
