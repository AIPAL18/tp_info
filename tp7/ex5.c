#include <stdio.h>
#include <stdlib.h>

int mystere(int n) {
    if (n == 1) return 0;
    if (n % 2 == 0) return 1 + mystere(n / 2);
    return 1 + mystere(n + 1);
}

/*
QU 1: Justifier que `mystere` termine pour n >= 1.

Soit v, le variant n
- Si n == 1: mystere s'arrête
- Sinon
            | 1 + m(n/2)  (n paire)
    m(n) =  |
            | 1 + m(n + 1) (n impaire)
    La limite de la fonction est donc 1.
Conclusion, mystere termine.
*/

int main(void)
{
    int n = 10000000;
    int max = mystere(n);
    for (int i = 1; i < n; ++i) {
        int k = mystere(i);
        if (k >= n) printf("    %d (%d) \n", k, i);
    }
    return 0;
}
