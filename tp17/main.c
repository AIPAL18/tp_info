#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define SIZE(t) sizeof(t)/sizeof(typeof(t[0]))

#define pprint(v) _Generic((v), \
    int: printf("%d", (v)), \
    double: printf("%.2d", (v)), \
    *double: printf("%.2d", *(v)), \
)

void concat(char* f, char* a) {}

void display_tab(void* tab, size_t size) {
    printf("[");
    for (size_t i = 0; i < size - 1; ++i) {
        pprint(tab[i]);
        printf(", ");
    }
    pprint(tab[size - 1]);
    printf("]\n");
    fflush(stdout);
}

// void qsort(void *base, size_t nmemb, size_t size,
//     int (*compar)(const void *, const void *));

// absolute ascending order (integers)
int icompare_absasc(const void* a, const void* b) {
    return abs(*((int*)a)) - abs(*((int*)b));
}

void tri_4(int* tab, int n) {
    qsort(tab, n, sizeof(int), icompare_absasc);
}

// absolute ascending order (floats)
int fcompare_absasc(const void* a, const void* b) {
    return fabs(**((double**)a)) - fabs(**((double**)b));
}

void tri_5(double** tab, int n) {
    qsort(tab, n, sizeof(float), fcompare_absasc);
}

int main(void)
{
    int tab1[] = {0, 1, 2, 3, 4, 5};
    int tab2[] = {5, 4, -3, 2, 1, 0};
    double tab3[] = {0.0, 1.0, 2.0, 3.0, 4.0, 5.0};
    double tab4[] = {5.0, 4.0, -3.0, 2.0, 1.0, 0.0};

    tri_4(tab1, SIZE(tab1));
    display_tab(tab1, SIZE(tab1));

    tri_4(tab2, SIZE(tab2));
    display_tab(tab2, SIZE(tab2));

    tri_5(tab3, SIZE(tab3));
    display_tab(tab3, SIZE(tab3));

    tri_5(tab4, SIZE(tab4));
    display_tab(tab4, SIZE(tab4));
    
    printf("Hello, World!\n");
    return 0;
}
