#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

bool est_dans(int x, int* T, int n)
{
    for (int i = 0; i < n; ++i) {
        if (T[i] == x) return true;
    }
    return false;
}

/*
QU 2: Quel est le cas de base de la recherche dichotomique ?
Le cas de base est qu'on est trouver l'élément.
*/

bool est_dans_rec(int x, int* T, int n)
{
    if (n <= 0) return false;
    int m = n / 2;
    if (x == T[m]) return true;
    if (x < T[m]) return est_dans_rec(x, T, n - m - 1);
    return est_dans_rec(x, &T[m + 1], n - m - 1);
}

bool est_dans_iter(int x, int* T, int n) {
    int l = 0;
    int r = n - 1;
    while (l <= r)
    {
        int m = l + (r - l + 1) / 2;
        if (x == T[m]) return true;
        else if (x < T[m]) r = m - 1; 
        else l = m + 1;
    }
    return false;
    
}

int main(void)
{
    int n = 5;
    int T[5] = {1, 2, 3, 4, 5};
    
    printf("est_dans: %d\n", est_dans(2, T, n));
    printf("est_dans: %d\n", est_dans_rec(2, T, n));
    printf("est_dans: %d\n", est_dans_iter(2, T, n));

    printf("est_dans: %d\n", est_dans(8, T, n));
    printf("est_dans: %d\n", est_dans_rec(8, T, n));
    printf("est_dans: %d\n", est_dans_iter(8, T, n));

    printf("est_dans: %d\n", est_dans(0, T, n));
    printf("est_dans: %d\n", est_dans_rec(0, T, n));
    printf("est_dans: %d\n", est_dans_iter(0, T, n));

    printf("est_dans: %d\n", est_dans(5, T, n));
    printf("est_dans: %d\n", est_dans_rec(5, T, n));
    printf("est_dans: %d\n", est_dans_iter(5, T, n));
    
    return 0;
}
