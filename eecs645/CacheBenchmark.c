//*****************************************************************************
//
//	Program:		CacheBenchmark.c
//	Author:			Gary J. Minden <gminden@ittc.ku.edu>
//	Date:			2017-02-03 (B70203)
//	Description:	A simple cache benchmark
//
//  Updates:
//
//      B40922 -- Added a loop with random indices
//
//*****************************************************************************	

#include <stdio.h>
#include <time.h>
#include <strings.h>
#include <stdlib.h>
#include <unistd.h>

#define NbrPlanes  64
#define NbrRows  1024
#define NbrCols  1024
#define NbrIterations 10

int32_t Data[NbrPlanes][NbrRows][NbrCols];

int main (int argc, const char * argv[])
{
    int32_t     I, J, K = 0;            //  Data indicies
    int32_t     L = 0;                  //  Iterations
    int64_t     IRand, JRand, KRand;    //  Random indicies
    int32_t     X = 0;

    time_t StartTime, EndTime;
    
    printf(">>CacheBenchmark: Starting.\n");
    
    time( &StartTime );
    printf( ">>CacheBenchmark: Start: Time = %12ld   %s\n", StartTime, ctime( &StartTime ) );
    
    printf( ">>Size of int: %ld; Size of long int: %ld; Size of time_t: %ld\n",
           sizeof( int ), sizeof( long int ), sizeof( time_t ) );
    
    //
    //  Step through Data to (hopefully) insure the array is in main memory.
    //
    for( I = 0; I < NbrPlanes; I++ ) {
        for( J = 0; J < NbrRows; J++ ) {
            for( K = 0; K < NbrCols; K++ ) {
                X = Data[I][J][K];
            }
        }
    }
    
    //
    //  Index through Data L times, last index varies fastest
    //
    sleep( 1 );								//	Start timing on a Second
    time( &StartTime );
    for( L = 0; L < NbrIterations; L++ ) {
        for( I = 0; I < NbrPlanes; I++ ) {
            for( J = 0; J < NbrRows; J++ ) {
                for( K = 0; K < NbrCols; K++ ) {
                    X = Data[I][J][K];
                }
            }
        }
    }
    time( &EndTime );
    
    printf( ">>>>X: %d\n", X );				//	Used to insure 'for' loops execute
    
    printf( ">>CacheBenchmark: Last index fastest; Start: %12ld; End: %12ld; Delta: %12ld\n",
    				StartTime, EndTime, (EndTime - StartTime) );

    //
    //  Index through Data L times, first index varies fastest
    //
    sleep( 1 );
    time( &StartTime );
    for( L = 0; L < NbrIterations; L++ ) {
        for( K = 0; K < NbrCols; K++ ) {
            for( J = 0; J < NbrRows; J++ ) {
                for( I = 0; I < NbrPlanes; I++ ) {
                    X = Data[I][J][K];
                }
            }
        }
    }
    
    time( &EndTime );
    
    printf( ">>>>X: %d\n", X );				//	Used to insure 'for' loops execute
    
    printf( ">>CacheBenchmark: First index fastest; Start: %12ld; End: %12ld; Delta: %12ld\n",
    				StartTime, EndTime, (EndTime - StartTime) );
    
    //
    //  Measure time to generate random indicies without data access
    //
    sleep( 1 );
    time( &StartTime );
    for( L = 0; L < NbrIterations; L++ ) {
        for( K = 0; K < NbrCols; K++ ) {
            for( J = 0; J < NbrRows; J++ ) {
                for( I = 0; I < NbrPlanes; I++ ) {
                    IRand = (random() % NbrPlanes);
                    KRand = (random() % NbrCols);
                    JRand = (random() % NbrRows);
                    
                }
            }
        }
    }
    
    time( &EndTime );
    
    printf( ">>>>X: %d\n", X );				//	Used to insure 'for' loops execute
    
    printf( ">>CacheBenchmark: Random indicies, no data access; Start: %12ld; End: %12ld; Delta: %12ld\n",
           StartTime, EndTime, (EndTime - StartTime) );

    //
    //  Index through Data L times, random indices
    //
    sleep( 1 );
    time( &StartTime );
    for( L = 0; L < NbrIterations; L++ ) {
        for( K = 0; K < NbrCols; K++ ) {
            for( J = 0; J < NbrRows; J++ ) {
                for( I = 0; I < NbrPlanes; I++ ) {
                    IRand = (random() % NbrPlanes);
                    KRand = (random() % NbrCols);
                    JRand = (random() % NbrRows);
                    
                    X = Data[IRand][JRand][KRand];
                }
            }
        }
    }
    
    time( &EndTime );
    
    printf( ">>>>X: %d\n", X );				//	Used to insure 'for' loops execute
    
    printf( ">>CacheBenchmark: Random indicies, with data access; Start: %12ld; End: %12ld; Delta: %12ld\n",
           StartTime, EndTime, (EndTime - StartTime) );

    return( 1 );
    
}

