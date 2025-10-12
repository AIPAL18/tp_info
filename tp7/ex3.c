#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <stdlib.h>

/*
QU 3: Dessiner l’arbre d’appels récursifs de l’appel binom(5, 2)

Il y a beaucoup de redondance dans les calculs
*/

int binom(int n, int k);
int nb_appels_binom(int n, int k);
int** creer_matrice_nulle(int m);
void detruire_matrice(int** mat, int m);
int binom_memo(int n, int k);
int binom_memo_mieux(int n, int k);

int main(void)
{
    printf("%d\n", binom(10, 5));
    printf("%d\n", binom_memo(10, 5));
    printf("%d\n", binom_memo_mieux(10, 5));
    printf("en %d appels\n", nb_appels_binom(10, 5));
    return 0;
}

int binom_memo_mieux(int n, int k)
{
    int* mat = calloc(n + 1, sizeof(int));
    for (int i = 0; i < n + 1; ++i) {
        mat[0] = 1;
        int prev = mat[0];
        for (int j = 1; j < i; ++j) {
            int temp = mat[j];
            mat[j] = prev + mat[j];
            prev = temp;
        }
        mat[i] = 1;
    }
    int v = mat[k];
    free(mat);
    return v;
}

int binom_memo(int n, int k)
{
    int** mat = creer_matrice_nulle(n + 1);
    for (int i = 0; i < n + 1; ++i) {
        mat[i][0] = 1;
        for (int j = 1; j < i; ++j) {
            mat[i][j] = mat[i - 1][j - 1];
            mat[i][j] += mat[i - 1][j];
        }
        mat[i][i] = 1;
    }
    int v = mat[n][k];
    // for (int i = 0; i < n + 1; ++i){
    //     for (int j = 0; i <= j; ++j) {
            
    //     }
    // }
    detruire_matrice(mat, n + 1);
    return v;
}

void detruire_matrice(int** mat, int m)
{
    if (mat == NULL) return;
    for (int i = 0; i < m; ++i) {
        free(mat[i]);
    }
    free(mat);
}

int** creer_matrice_nulle(int m)
{
    int** matrice = calloc(m, sizeof(int*));
    assert(matrice != NULL && "Buy more RAM lol");
    for (int i = 0; i < m; ++i) {
        matrice[i] = calloc(m, sizeof(int));
        assert(matrice[i] != NULL && "Buy more RAM lol");
    }
    return matrice;
}

int nb_appels_binom(int n, int k)
{
    return 2 * binom(n, k) - 1;
}

int binom(int n, int k)
{
    assert(0 <= k && k <= n);
   if (k == n) return 1;
   if (k == 0) return 1;
   return binom(n - 1, k - 1) + binom(n - 1, k);
}
