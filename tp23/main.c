#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

typedef struct {
    int taille; // nombre d'éléments dans le tas
    int capacite; // taille du tableau tab
    int *tab;
} tas;

tas* creer_tas(int capacite) {
    tas* t = calloc(1, sizeof(tas));
    t->capacite = capacite;
    t->taille = 0;
    t->tab = calloc((size_t)(t->capacite), sizeof(int));
    return t;
}

void free_tas(tas* t) {
    free(t->tab);
    free(t);
}

inline bool est_vide(tas* t) {return t->taille == 0;}

inline int pere(int i) {return (i - 1) / 2;}

inline int gauche(int i) {return 2*i + 1;}

inline int droite(int i) {return 2*i + 2;}

void echanger(tas* t, int i, int j) {
    int temp = t->tab[i];
    t->tab[i] = t->tab[j];
    t->tab[j] = temp;
}

void percoler_bas(tas* t, int i) {
    bool is_d = 0;
    while (t->tab[i] < gauche(i) || (is_d = t->tab[i] < droite(i))) {
        if (is_d) {
            echanger(t, i, droite(i));
            i = droite(i);
        }
        else {
            echanger(t, i, gauche(i));
            i = gauche(i);
        }
    }
}

void percoler_haut(tas* t, int i) {
    while (t->tab[i] > t->tab[pere(i)]) {
        echanger(t, i, pere(i));
        i = pere(i);
    }
}

void inserer(tas* t, int x) {
    assert(t->taille < t->capacite && "Full");
    t->tab[t->taille] = x;
    t->taille++;
    percoler_haut(t, t->taille - 1);
}

// int extraire_max(tas* t) {
    
// }

int m_log2(int x) {
    int result = 0;
    while (x >>= 1) result++;
    return result;
}

void display(tas* t) {
    int level = -1;
    int height = m_log2(t->taille);
    printf("taille: %d\n\n", t->taille);
    int size_of_element = 2;
    
    for (int i = 0; i < t->taille; ++i) {
        if (i == 0 || !(i+1 >= (1 << level) && (i+1) <= ((1 << (level+1)) - 1))) {
            level++;
            printf("\n");
            for (int j = 0; j < size_of_element * ((1<<(height-level))-1); j++) printf(" ");
        }
        printf("%02d", t->tab[i]);
        for (int j = 0; j < size_of_element * ((1<<(height-(level - 1)))-1); j++) printf(" ");
    }
    printf("\n");
}

int main(void) {
    tas* t = creer_tas(10);
    
    inserer(t, 10);
    inserer(t, 20);
    inserer(t, 5);
    inserer(t, 9);

    display(t);
    
    free_tas(t);

    return 0;
}
