int funca( int a ) {
    return a*a;
}

int funcb( int b ) {
    return funca( b );
}

int main( int argc, char *argv[] ) {
    int a;
    int b;

    a = funca( 5 );
    b = funcb( 5 );
}
