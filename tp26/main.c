#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include <stdarg.h>

void swap(int* i, int* j) {
    int temp = *j;
    *j = *i;
    *i = temp;
}

int partitionner_lomuto(int* t, int len, int pivot) {
    int i_1 = 0;
    int p = t[len - 1];
    swap(&t[pivot], &t[len - 1]);
    for (int i_2 = 0; i_2 < len; ++i_2) {
        if (t[i_2] <= t[len - 1]) {
            swap(&t[i_1], &t[i_2]);
            ++i_1;
        }
    }
    swap(&t[len - 1], &t[i_1]);
    return  i_1 - 1;
}

int pivot_lomuto(int* t, int len) {
    (void)t;
    return  len - 1;
}

int partitionner_hoare(int* t, int len, int pivot) {
    int i_1 = 1;
    int i_2 = len;
    swap(t, &t[pivot]);
    while (i_1 < i_2) {
        if (t[i_1] > t[0]) {
            swap(&t[i_1], &t[i_2 - 1]);
            --i_2;
        }
        else ++i_1;
    }
    swap(t, &t[i_1-1]);
    return  i_1;
}

int pivot_hoare(int* t, int len) {
    (void)t; (void)len;
    return 0;
}

void tri_rapide(int* t, int len, int (get_pivot)(int*, int), int (partitionner)(int*, int, int)) {
    if (len <= 1) return;
    int pivot = get_pivot(t, len);
    int p2 = partitionner(t, len, pivot);
    tri_rapide(t, p2, get_pivot, partitionner);
    tri_rapide(&t[p2 + 1], len - p2 - 1, get_pivot, partitionner);
}

void display(int t[], int len) {
    printf("[");
    for (int i = 0; i < len; ++ i) {
        printf("%d, ", t[i]);
    }
    printf("]\n");
}

int** tableau_aleatoire(int size, int n) {
    int** t = malloc((size_t)n * sizeof(int*));
    for (int i = 0; i < n; ++i) {
        t[i] = malloc((size_t)size *sizeof(int));
        for (int j = 0; j < size; ++j) {
            if (i > 0)
                t[i][j] = t[i-1][j];
            else
                t[i][j] = rand()>>20;
        }
    }
    return t;
}

double test_temps_execution(int* t, int n, void (f)(int*, int)) {
    clock_t clock();

    f(t, n);
    display(t, n);

    clock_t diff = clock();
    return ((double)diff / CLOCKS_PER_SEC);
}

typedef struct {
    void (*f)(int*, int);
    char* name;
} item;

void test_pool(int size, int n, ...) {
    int** t = tableau_aleatoire(size, n);
    va_list args;
    va_start(args, n);  
    for (int k = 0; k < n; k++) {
        item i = va_arg(args, item);
        double s = test_temps_execution(t[k], size, i.f);
        printf("%s : %f\n", i.name, s);
    }
    va_end(args);
    for (int i = 0; i < n; ++i) {
        free(t[i]);
    }
    free(t);
}

void tri_rapide_2(int* t, int len) {
    tri_rapide(t, len, pivot_lomuto, partitionner_lomuto);
}

int compar(const void* a, const void* b) {
    return *(int*)a - *(int*)b;
}

void qsort_2(int* t, int len) {
    qsort(t, (size_t)len, sizeof(int), compar);
}

int main(void) {
    unsigned int seed = 1773663735;//(unsigned int)time(NULL);
    printf("%d\n", seed);
    srand(seed);

    // int t[] = {9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 20, 25, 18, 30};
    // int len = sizeof(t)/sizeof(int);
    
    // display(t, len);
    // tri_rapide(t, len, pivot_lomuto, partitionner_lomuto);
    // display(t, len);

    // ------------------------------------------------------

    int** super = tableau_aleatoire(20, 1);
    int t2[] = {1292, 1824, 1722, 1641, 1515, 1406, 1321, 163, 1134, 453, 1913, 1083, 295, 811, 131, 1889, 1719, 1185, 1605, 1540}; // super[0];
    int len2 = 20;
    
    display(t2, len2);
    tri_rapide(t2, len2, pivot_lomuto, partitionner_lomuto);
    display(t2, len2);
    qsort(t2, (size_t)len2, sizeof(int), compar);
    display(t2, len2);
    
    free(super[0]); free(super);

    // item tests[] = {
    //     (item){
    //     .f = &tri_rapide_2,
    //     .name = "tri rapide",
    //     },
    //     (item){
    //     .f = &qsort_2,
    //     .name = "qsort",
    //     }
    // };
    
    // test_pool(10, 
    //     sizeof(tests)/sizeof(item),
    //     tests[0],
    //     tests[1]
    // );

    // [743, 945, 1743, 1449, 1809, 783, 420, 713, 756, 680, 1052, 1121, 1875, 1220, 1480, 1724, 1121, 968, 96, 691, ]

    return  0;
}
