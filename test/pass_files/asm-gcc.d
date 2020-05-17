module asm_gcc;

ref T store(T)();

enum asm1 = "mov %0, %0;";
string asm2(int i) { return "mov %0, %0;"; }

void main()
{
    int var1, var2, var3, var4;
    int* ptr1;

    asm
    {
        // Some tests as found in dmd's iasmgcc.d
        "nop";
        asm1;
        asm2(1);
        mixin(`"repne"`, `~ "scasb"`);

        // GCC examples
        "notl %[iov]" : [iov] "=r" (var1) : "0" (var2)   ;
        ;
        "mov %1, %0\n\t"
        "add $1, %0" : "=r" (var1) : "r" (var2)   ;

        // DRuntime
        "cpuid" : "=a" (var1), "=c" (var2), "=d" (var3) : "a" (0x8000_0006) : "ebx"    ;

        // Deprecated: Missing parens
        "cpuid" : "=a" var1, "=b" var2 : "a" 0x8000_001E : "ecx", "edx";

        "str x29, %0" : "=m" (var1)    ;

        "mrs %0, cntvct_el0" : "=r" *ptr1;

        "mov %1, %0" :  : "r" (var2) : "cc" : LCarry ;
        "mov %0, %0" :  : "r" (var2) : "cc" ;

        "mov %0, %0" : "=r" (*ptr1) : "r" (store!int = 1) : "cc";
    }

    LCarry:
    asm  /*goto*/ {
        "btl %1, %0\n\t"
        "jc %l2"
        : /* No outputs. */
        : "r" (var1), "r" (var2)
        : "cc"
        : LCarry
        ;
    }

    asm {
        ;
        ;
        "jmp LCarry" : : : : LCarry ;
    }
}
