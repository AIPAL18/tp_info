#include <stdio.h>
#include <stdlib.h>
/*
Un tableau trié dans l'ordre décroissant de taille n, a un nombre d'inversions
de (n-1)(n-2)/2 
*/

void fusion(int t[], int len) {
    int* resultat = malloc ((size_t)len * sizeof (int));
    int i = 0;
    int j = len / 2;

    for (int k = 0 ; k < len ; k++) {
        if (j == len || (i != len / 2 && t[i] <= t[j])) {
            resultat[k] = t[i];
            i++;
        } else {
            resultat[k] = t[j];
            j++;
        }
    }

    for (int i = 0 ; i < len ; i++) t[i] = resultat[i];

    free (resultat);
}

void tri_fusion(int* t, int len){
    if (len <= 1) return; 
    else {
        tri_fusion(t , len/2); 
        tri_fusion(&t[len/2], len - len/2);
        fusion(t, len);
    }
}

int nombre_inversions_naif(int t[], int n) {
    int count = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = i; j < n; ++j) {
            if (t[i] > t[j]) count++;
        }
    }
    return count;
}

int nombre_inversions_croisees(int t1[], int n, int t2[], int m) {

    int r = 0;
    int i = 0;
    int j = 0;
    while (i < n && j < m) {
        if (t1[i] > t2[j]) {
            r += n-i;
            j++;
        }
        else {
            i++;
        }
    }
    return r;
}

int nombre_inversions_aux(int t[], int deb, int fin) {
    // int* c1 = malloc(sizeof(int)*(size_t)n);
    // int* c2 = malloc(sizeof(int)*(size_t)m);
    // for (int i = 0; i < n; ++i) {
    //     c1[i] = t1[i];
    // }
    // for (int i = 0; i < n; ++i) {
    //     c2[i] = t2[i];
    // }
    // tri_fusion(c1, n);
    // tri_fusion(c2, m);


    // free(c1);
    // free(c2);
}

int main(void) {
    int t[] = {4, 3, 2, 1, 0};
    printf("%d\n", nombre_inversions_naif(t, sizeof(t)/sizeof(int)));
    int t1[] = {2, 4, 6, 8};
    int t2[] = {1, 3, 5, 7};
    
    printf("%d\n", nombre_inversions_croisees(t1, 4, t2, 4));
    return 0;
}
