#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <string.h>

extern void yyparse(void*);
extern void yylex_init(void**);
extern void yyset_in(FILE*,void*);
extern void yylex_destroy( void* );
extern FILE* yyin;

void mkparent( const char *path ) {
    // Duplicate the path so that we can modify it
    char *dir = strdup(path);
    if( dir == NULL ) {
        perror( "Could not determine parent directory" );
        exit( 1 );
    }

    // Search for the end of the path
    char *cur = dir;
    while( *cur != 0 ) cur++;

    // Search backwards for the first path separator
    while( --cur > dir ) {
        if( *cur == '/' && *(cur-1) != '\\' ) {
            *cur = 0;
            break;
        }
    }

    // If there was no parent then finish
    if( cur == dir ) {
        free( dir );
        return;
    }

    // If the directory already exists then finish
    struct stat st;
    if( stat(dir,&st) == 0 ) {
        free( dir );
        return;
    }

    // Make the parent of the parent path
    mkparent( dir );

    // Make the parent
    if( mkdir( dir, 0755 ) != 0 ) {
        free( dir );
        perror( "Could not make parent directory" );
        exit( 1 );
    }

    // Free up the duplicated path
    free( dir );
}

int main( int argc, const char *argv[] ) {
    void *scanner;

    FILE *input;
    if( argc >= 2 ) input = fopen( argv[1], "r" );
    else            input = stdin;

    if( input == NULL ) {
        perror( "cannot open input file" );
        exit( 1 );
    }

    if( argc >= 3 )  {
        mkparent( argv[2] );
        FILE *res = freopen( argv[2], "w", stdout );
        if( res == NULL ) {
            perror( "cannot open output file" );
            exit( 1 );
        }
    }

    yylex_init( &scanner );
    yyset_in( input, scanner );
    yyparse( scanner );
    yylex_destroy( scanner );

    exit(0);
}
