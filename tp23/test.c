#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int main(void) {
    const int taille = 10*10*10*10*10*10*10*10;
    printf("calloc\n");
    int64_t* tab = calloc(taille, sizeof(int64_t));
    printf("start\n");
    for (int i = 0; i < taille; ++i) {
        tab[i] = 0;
    }
    printf("end\n");
    return 0;
}
