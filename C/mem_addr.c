#include <stdio.h>
#include <stdlib.h>

int n = 10;
const int n1 = 20;
int m;

int main() {
    int s = 7;
    static int s1 = 30;
    char *p = (char*)malloc(20);

    printf("global variable address: \t%p\n", &n);
    printf("const global address: \t%p\n", &n1);
    printf("golbal unintialized address: \t%p\n", &m);
    printf("stack variable address: \t%p\n", &s);
    printf("static variable address: \t%p\n", &s1);
    printf("heap variable address: \t%p\n", &p);

    pause();
}
