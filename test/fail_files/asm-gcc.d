void main()
{
    asm
    {
        "mov A B";
        ;
        "mov A B" : (a);
        "mov A B" : xx "rw" (a);

        "mov A B" : "rw" (a), ;
        "mov A B" : "rw" (a), : ;
        "mov A B" : "rw" (a) : , : ;

        "mov A B" : "rw" (a) : "r" (b) : this;


        "mov A B" : "rw" (a) : "r" (b) : "xxx" : 0;

        "mov A B" : "rw" (a) : "r" (b) : "xxx" : LEnd ;
    }
}