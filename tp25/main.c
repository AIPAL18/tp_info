#include <assert.h>
#include <stdio.h>
#include <stdbool.h>

#define min(a, b) \
    ((a) >= (b) ? (b) : (a))

#define max(a, b) \
    ((a) <= (b) ? (b) : (a))

int grille_exemple[9][9] = {
    {2, 5, 0, 0, 3, 0, 9, 0, 1},
    {0, 1, 0, 0, 0, 4, 0, 0, 0},
    {4, 0, 7, 0, 0, 0, 2, 0, 8},
    {0, 0, 5, 2, 0, 0, 0, 0, 0},
    {0, 0, 0, 0, 9, 8, 1, 0, 0},
    {0, 4, 0, 0, 0, 3, 0, 0, 0},
    {0, 0, 0, 3, 6, 0, 0, 7, 2},
    {0, 7, 0, 0, 0, 0, 0, 0, 3},
    {9, 0, 3, 0, 0, 0, 6, 0, 4}};


void affiche_grille(int grille[9][9]) {
    int h = 9;
    int w = 9;
    printf("┌───┬───┬───┬───┬───┬───┬───┬───┬───┐\n");
    while (h --> 0) {
        while (w --> 0) {
            printf("│ %d ", grille[9 - h - 1][9 - w - 1]);
        }
        w = 9;
        printf("│\n");
        if (h > 0)
            printf("├───┼───┼───┼───┼───┼───┼───┼───┼───┤\n");
    }
    printf("└───┴───┴───┴───┴───┴───┴───┴───┴───┘\n\n");
}


void suivant(int grille[9][9], int i, int j, int* x, int* y) {
    if (grille[i][j] == 0) {
        *x = i;
        *y = j;
        return;
    }
    j++;
    if (j >= 8) {
        i++;
        j = j % 9;
    }
    suivant(grille, i, j, x, y);
    *x = -1;
    *y = -1;
    return;
}

bool mfalse(char* message) {
    printf("%s\n", message);
    return false;
}

bool valide(int grille[9][9], int i, int j) {
    assert(grille[i][j] != 0);

    // check square
    int snap_h = 3 * (i / 3);
    int snap_w = 3 * (j / 3);

    for (int h = snap_h; h < snap_h + 3; ++h) {
        for (int w = snap_w; w < snap_w + 3; ++w) {
            if ((i != h || j != w) && grille[i][j] == grille[h][w]) return false;
        }
    }

    // check row
    for (int w = 0; w < 8; ++w) {
        if (w != j && grille[i][j] == grille[i][w]) return false;
    }

    // check row
    for (int h = 0; h < 8; ++h) {
        if (h != i && grille[i][j] == grille[h][j]) return false;
    }

    return true;
}

bool resout_aux(int grille[9][9], int i, int j) {
    // assert(i >= 0 && j >= 0);
    // assert(grille[i][j] == 0);
    // for (int c = 1; c <= 9; ++c) {
    //     grille[i][j] = c;
    //     if (valide(grille, i, j)) {
    //         int x, y;
    //         suivant(grille, i, j, &x, &y);
    //         if (resout_aux(grille, x, y)) return true;
    //     }
    // }
    // grille[i][j] = 0;
    // return false;
    suivant(grille, i, j, &i, &j);
    if (i==-1 && j == -1) return true;
    for (int c = 1; c<10; c++)
    {
        grille[i][j] = c;
        if (valide(grille, i, j))
        {
            if (resout_aux(grille, i, j)) return true;
        }

    }
    grille[i][j] = 0;
    return false;
}

bool resout(int grille[9][9]) {
    // int x = 0;
    // int y = 0;
    // suivant(grille, 0, 0, &x, &y);

    return resout_aux(grille, 0, 0);
}

int main(void) {
    affiche_grille(grille_exemple);
    // int x = 0;
    // int y = 0;
    // suivant(grille_exemple, 0, 0, &x, &y);
    // assert(x == 0 && y == 2);
    // suivant(grille_exemple, 0, 3, &x, &y);
    // assert(x == 0 && y == 3);
    // suivant(grille_exemple, 0, 4, &x, &y);
    // assert(x == 0 && y == 5);

    // assert(valide(grille_exemple, 0, 0));
    // grille_exemple[0][2] = 2;
    // assert(!valide(grille_exemple, 0, 0));
    // grille_exemple[0][2] = 0;
    // grille_exemple[7][0] = 2;
    // assert(!valide(grille_exemple, 0, 0));
    // grille_exemple[7][0] = 9;
    // grille_exemple[0][7] = 2;
    // assert(!valide(grille_exemple, 0, 0));
    // grille_exemple[0][7] = 0;
    // grille_exemple[1][2] = 2;
    // assert(!valide(grille_exemple, 0, 0));
    // grille_exemple[1][2] = 0;

    // bool r = resout(grille_exemple);
    // printf("r=%d (t: %d, f: %d)\n", r, true, false);
    // affiche_grille(grille_exemple);

    int x = 0;
    int y = -1;
    while (x >= 0 || y >= 0) {
        suivant(grille_exemple, x, y+1, &x, &y);
        printf("(%d, %d)\n", x, y);
    }

    return 0;
}