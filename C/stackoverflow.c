#include <stdio.h>
#include <stdlib.h>

#define BITS_PER_BYTE 8
#define WORD (BITS_PER_BYTE * sizeof(unsigned int))
#define MASK WORD-1
#define SHIFT 5
#define N (unsigned int)(~0)

//unsigned int ignore_bitmap[1+N/WORD];

int main(){
    unsigned int ignore_bitmap[1+N/WORD];
    unsigned int len = 1 + N/WORD;

    printf("%u\n", len);

    return 0;
}

