#include <assert.h>
#include <stdbool.h>
#include <stdio.h>  // printf
#include <stdlib.h> // malloc et rand
#include <time.h>   // time, pour la seed du générateur de nombres aléatoires
#define CHECK_ALLOC_IMPLEMENTATION
#include "safe_alloc.h"

void affiche_tableau(int arr[], int n) {
  for (int i = 0; i < n; i++) {
      printf("%2d ", arr[i]);
  }
  printf("\n");
}

int* genere_tableau_aleatoire(int n) {
  int* t = malloc(sizeof(int) * (size_t)n);
  for (int i = 0; i < n; i++) {
      t[i] = rand() % 100; // On se limite à des nombres entre 0 et 99 pour être facile à lire
  }
  return t;
}

bool est_trie(int* t, int n) {
  for (int i = 0; i < n - 1; i++) {
      if (t[i] > t[i + 1]) {
          return false;
      }
  }
  return true;
}

void fusion(int t[], int len) {
    assert(len >= 2);
    int i = 0;
    int j = len/2;
    int* c = my_calloc((size_t)len, sizeof(int));
    for (int k = 0; k < len; ++k) {
        if (t[i] <= t[j] && i < len/2) {
            c[k] = t[i];
            i++;
        }
        else if (j < len) {
            c[k] = t[j];
            j++;
        }
        else printf("hahha!!\n");
    }
    for (int k = 0; k < len; ++k) {
        t[k] = c[k];
    }
    my_free(c);
}

void tri_fusion(int t[], int len) {
    if (len == 1 || len == 0) {
        printf("fin\n");
        return;
    }
    fflush(stdout);
    if (len % 2 == 0) {
        printf("%d, %d, %d - ", len, 0, len/2);
        printf("%d, %d, %d\n", len, len/2, len/2);
        tri_fusion(t, len/2);
        tri_fusion(&t[len/2], len/2);
    } else {
        printf("%d, %d, %d - ", len, 0, (len - 1)/2);
        printf("%d, %d, %d\n", len, (len+1)/2, (len + 1)/2 - 1);
        tri_fusion(t, (len - 1)/2);
        tri_fusion(&t[(len+1)/2], (len + 1)/2 - 1);
    }
    fusion(t, len);
}

int main() {
    int n = 10;
    srand((unsigned int)time(NULL));
    int* t = genere_tableau_aleatoire(n);
    affiche_tableau(t, n);
    tri_fusion(t, n);
    affiche_tableau(t, n);
    assert(est_trie(t, n));
    free(t);
    return 0;
}