#include <iostream>
#include "Main_stub.h"

int main(int argc, char *argv[])
{
    std::cout << "hello\n";
    hs_init(&argc, &argv);
    main2() ;
    hs_exit();
    return 0;
}