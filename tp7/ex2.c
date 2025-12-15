/*
Mouvement autorisé: vers la gauche, vers le bas
QU 1: Pour quelles valeurs de (m, n) n’y a-t-il qu’une seule façon de se déplacer jusqu’à la case (0, 0) ?
Pour (0, 1) et (1, 0).
QU 2:Exprimer C(m, n) en fonction de C(m − 1, n) et C(m, n − 1) dans les autres cas
C(m, n) = C(max(m - 1, 0), n) + C(m, max(n - 1, 0))
*/

#include <stdio.h>
#include <stdbool.h>

int nb_chemins(int m, int n)
{
    /*
    NOTE: if (m == 0 || n == 0) return m != n
        | m | n | r |
        |---|---|---|
        | 0 | 0 | 0 |
        | 0 | 1 | 1 |
        | 1 | 0 | 1 |
        |>=1|>=1| ∅ |
    */
    if (m == 0 || n == 0) return m != n;
    return nb_chemins(m - 1, n) + nb_chemins(m, n - 1);
}

int main(void)
{
    printf("%d\n", nb_chemins(10, 10));
    return 0;
}
