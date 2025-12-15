#include <stdio.h>
#include <stdbool.h>

#define min(a, b) \
    (((a) >= (b)) ? (a) : (b))

int u(int n) {
    if (n == 0) return 1;
    return 3 * u(n - 1) + 2;
}

int u_iter(int n) {
    int v = 1;
    
    for (int i = 1; i < n; ++i) {
        v += 3 * v + 2;
    }
    
    return v;
}

int PGCD(int a, int b) {
    int res = min(a, b);
    
    while (res > 1) {
        if (a % res == 0 && b % res == 0)
            break;
        res--;
    }
    return res;
}

int PGCD_rec(int a, int b) {
    if (a == 0)
        return b;
    if (b == 0)
        return a;
    if (a == b)
        return a;
    if (a > b)
      return PGCD_rec(a - b, b);
    return PGCD_rec(a, b - a);
}

int fib_rec(int n) {
    if (n == 0) return 0;
    if (n == 1) return 1;
    return fib_rec(n - 1) + fib_rec(n - 2);
}

int b(int x);

int a(int x) {
    if (x == 0) return 0;
    return b(x);
}

int b(int x) {
    if (x == 0) return 1;
    return a(x) + b(x);
}

typedef struct {int fst; int snd;} Couple;


int main(void)
{
    printf("%d\n", fib_rec(42));
    return 0;
}
