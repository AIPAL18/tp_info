#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>*


typedef struct {
    int taille; // nombre d'éléments dans le tas
    int capacite; // taille du tableau tab
    int *tab;
} tas;

tas* creer_tas(int capacite){
    tas* r = malloc(sizeof(tas));
    r->tab = malloc(sizeof(int)* capacite);
    r->capacite = capacite;
    r->taille = 0;
    return r;
}

void free_tas(tas* t){
    free(t->tab);
    free(t);
}

bool est_vide(tas* t){
    return (t->taille == 0);
}

int pere(int i){
    return (i - 1) / 2;
}

int gauche(int i){
   return 2*i + 1; 
}

int droite(int i){
  return 2*i + 2;
}

void echanger(tas* t, int i, int j){
    int temp = t->tab[i];
    t->tab[i] = t->tab[j];
    t->tab[j] = temp;
}

void percole_bas(tas* t, int i) {
    assert(t->taille > i);
    if(t->taille > droite(i)) {
        int imax = t->tab[gauche(i)] > t->tab[droite(i)] ? gauche(i) : droite(i);
        if(t->tab[i] < t->tab[imax]) {
            echanger_tas(t, i, imax);
            percole_bas(t, imax);
        }
    }
    else if(t->taille > gauche(i) && t->tab[i] < t->tab[gauche(i)]) {
        echanger_tas(t, i, gauche(i));
        percole_bas(t, gauche(i));
    }
}

void percoler_haut(tas* t, int i){
  if(i > 0 && t -> tab [pere(i)] < t -> tab[i] ){
        echanger(t, pere(i), i);
        percoler_haut(t, pere(i));
    }
}

void inserer(tas* t, int x){
    assert(t->taille < t->capacite);
    t->tab[t->taille] = x;
    percole_haut(t, t->taille);
    t->taille = t->taille + 1;
}

int extraire_max(tas* t){
    assert(t->taille > 0);
    t->taille = t->taille -1;
    if (t->taille > 0) {
        echanger(t, 0, t->taille);
        percole_bas(t, 0);
    }
    return t->tab[t->taille];
}

void modifier(tas* t, int i, int x) {
    int old = t->tab[i];
    t->tab[i] = x;

    if (x > old) percoler_haut(t, i);
    else if (x < old) percoler_bas(t, i);
}

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

typedef struct {
    int taille; // nombre d'éléments dans la file
    int capacite; // taille des tableaux tab, valeurs et priorites
    int *priorites;
    int *valeurs;
    int *position;
} file_priorite;

file_priorite* creer_file_priorite(int capacite) {
    file_priorite* fp = calloc(1, sizeof(file_priorite));
    fp->capacite = capacite;
    fp->taille = 0;
    fp->priorites = calloc(capacite, sizeof(int));
    fp->valeurs = calloc(capacite, sizeof(int));
    fp->position = calloc(capacite, sizeof(int));
    return fp;
}

void free_file_priorite(file_priorite* f) {
    assert(f != NULL);
    assert(f->priorites != NULL);
    assert(f-> valeurs != NULL);
    assert(f->position != NULL);
    free(f->priorites);
    free(f->valeurs);
    free(f->position);
    free(f);
}

void echange_fp(file_priorite* f, int i, int j) {
    int pos_i = f->valeurs[i];
    int pos_j = f->valeurs[j];
    int v_tmp = f->valeurs[j];
    f->valeurs[j] = f->valeurs[i];
    f->valeurs[i] = v_tmp;
    int v_prio = f->priorites[j];
    f->priorites[j] = f->priorites[i];
    f->priorites[i] = v_prio;
    f->position[pos_i] = pos_j;
    f->position[pos_j] = pos_i;
}

void modifier_priorite(file_priorite* f, int v, int p) {
    
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
