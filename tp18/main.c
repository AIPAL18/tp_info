#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define idisplay_t(t, n) \
    printf("["); \
    for (int i = 0; i < n; ++i) {printf("%ld, ", (t)[i]);} \
    printf("]\n") \

typedef uint64_t ui;

// w
#define UI_SIZE 64
// p
#define BLOCK_SIZE 8
// radix
#define RADIX ???
#define BASE (1 << BLOCK_SIZE)

void copy(ui* in, ui* out, int n) {
    while (n-->0) out[n] = in[n];
}

void zero(int* tab, int n) {
    while (n-->0) tab[n] = 0;
}

ui get_digit(ui x, int k)
{
    for (int i=1; i<k*BASE; i=i*2) x = x>>1;
    return x%BASE;
}

int main(int argc, char const *argv[])
{
    ui tab[] = {1, 2, 3, 4};
    ui tab2[4] = {0};
    idisplay_t(tab, 4);
    copy(tab, tab2, 4);
    idisplay_t(tab2, 4);

    printf("%lx\n", get_digit(0x10, 1));
    
    return 0;
}
