#include <stdio.h>
#include <stdlib.h>

typedef struct
{
    int v;
    void* children;
} arbre;

arbre* new_arbre(int v) {
    arbre* a = calloc(1, sizeof(arbre));
    a->v = v;
    a->children = NULL;
    return a;
}

void free_arbre(arbre* a) {
    da_foreach(a->children, arbre, item) {
        free_arbre(item);
    }
    da_free(a->children);
    free(a);
}

int main(void) {
    arbre* a = new_arbre(20);
    printf("%d\n", a->v);
    da_append(a->children, new_arbre(10));

    free_arbre(a);

    return 0;
}
