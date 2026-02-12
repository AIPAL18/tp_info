#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

typedef uint32_t T;

uint64_t hash(T x, int p);


const uint8_t empty = 0;
const uint8_t occupied = 1;

struct bucket {
    uint8_t status;
    T element;
};
typedef struct bucket bucket;
struct set {
    int p;
    bucket *buckets;
    uint64_t nb_empty;
};
typedef struct set set;

set *set_new(void);
bool set_is_member(set *s, T x);
void set_add(set *s, T x);
void set_remove(set *s, T x);
void set_delete(set *s);

inline uint32_t p2(int p) {
    // expo rapide 
    return 1 << p;
}

uint64_t hash(T x, int p) {
    return x &(p2(p) - 1);
    // return x % p2(p);
}

set *empty_table(int p) {
    set* s = calloc(1, sizeof(set));
    assert(s != NULL && "Buy more RAM, lol)");
    s->p = p;
    s->buckets = calloc(p2(p), sizeof(bucket));
    assert(s->buckets != NULL && "Buy more RAM, lol)");
    s->nb_empty = p2(p);
    return s;
}

set *set_new(void) {
    return empty_table(1);
}

set *set_example(void) {
    set* s = empty_table(2);
    s->buckets[0].element = 1492;
    s->buckets[0].status = occupied;
    s->buckets[1].element = 1939;
    s->buckets[1].status = occupied;
    s->buckets[3].element = 1515;
    s->buckets[3].status = occupied;
    s->nb_empty = 1;
    return s;  
}

// Jean
uint64_t search(set *s, T x, bool *found) {
    int i = hash(x, p2(s->p));
    while (s->buckets[i].status == occupied) {
        if (s->buckets[i].element == x) {*found = true; return i;}
        i++;
        if (i >= p2(s->p)) {i &= (p2(s->p));}
    }
    *found = false; return i;
}

/*
Rappel:
    Hypothèse: Il y a au moins un bucket dont le status est "empty"

Soit dₙ(a, b) = (b - a) mod n avec n=2ᵖ
On note δ l'indice de la prochaine case vide.
C'est-à-dire:
    δ = min {dₙ(hₙ(x), i) | i∈⟦0;n-1⟧ l'indice d'une case vide}

Un variant est dₙ(i, δ) car
* C'est un entier (car i et δ le sont)
* Minoré par 0 par définition de δ et par condition de boucle
* Strictement décroissant.
En effet
- Cas i < δ:
    Soit i'=(i+1) mod n
    Or         δ < n donc i' = i+1
    donc     δ-i > δ-i' = δ-i-1
    donc dₙ(i, δ) > dₙ(i', δ)
    puis dₙ(i, δ) strictement décroissant
- Cas i > δ:
    Préliminaire On a i < n par definition de i 
    - Pour i∈⟦δ+1;n-2⟧
        Préliminaire:
            dₙ(i, δ) = (δ - i) mod n
                                    or (δ - i < 0) et (δ + n - i > 0)
            donc dₙ(i, δ) = δ + n - i
        On pose i'=(i+1) mod n = i+1
        i' > i
        donc dₙ(i', δ) = δ+n-i-1
             dₙ(i, δ)  = δ+n-i
        d'où dₙ(i, δ)>dₙ(i', δ)
    - Pour i=n-1
        i'= i+1 mod n = (n-1)+1 mod n = 0
        Donc δ-n+1 mod n = δ+1 > δ-0 mod n = δ
        Puis          dₙ(i, δ) > dₙ(i', δ)
    - Pour i∈⟦0;δ⟧
        idem au cas i < δ
    Ainsi dₙ(i, δ) strictement.

Conclusion: 

*/

int main(void) {
    assert(p2(1) == 1);
    assert(p2(2) == 4);
    assert(p2(3) == 8);
    hash(10, 20);
    hash(550, 15);
    // printf("%d\n");
    return 0;
}


