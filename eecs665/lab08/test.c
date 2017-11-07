#include <stdio.h>
#include <stdlib.h>

extern int tstconst();
extern void tstcall();

extern int tstadd(int,int);
extern int tstsub(int,int);
extern int tstmul(int,int);
extern int tstdiv(int,int);
extern int tstmod(int,int);
extern int tstshl(int,int);
extern int tstshr(int,int);

int main( int argc, const char *argv[] ) {
    
    printf( "10 = %d\n", tstconst() );
    printf( "12 + 6 = %d\n", tstadd(12,6) );
    printf( "12 - 6 = %d\n", tstsub(12,6) );
    printf( "12 * 6 = %d\n", tstmul(12,6) );
    printf( "12 / 6 = %d\n", tstdiv(12,6) );
    printf( "12 %% 5 = %d\n", tstmod(12,5) );
    printf( "12 << 1 = %d\n", tstshl(12,1) );
    printf( "12 >> 1 = %d\n", tstshr(12,1) );

    // FIXME: Currently, no assembly will be generated for tstcall()
    //	      For the second part of this lab, you will need to make
    //        sure the correct code is generated for this function.
    //        Also, you will need to make sure strings are handled
    //        correctly in order to prevent this function from crashing.
    //tstcall();

    return 0;
}
