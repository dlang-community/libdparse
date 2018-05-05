static if (true)
{
    template A()
    {
        // Err mssg Used to be gagged and parse OK
        enum a = 1
        enum b = 0;
    }
}
