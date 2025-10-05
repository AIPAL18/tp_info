#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define STBI_NO_FAILURE_STRINGS
#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_WRITE_IMPLEMENTATION
#define STBI_FAILURE_USERMSG
#include "stb_image.h"
#include "stb_image_write.h"
#include "seam_carving.h"

#define min(a, b)\
    (a) <= (b) ? (a) : (b)

#define max(a, b)\
    (a) >= (b) ? (a) : (b)

#define abs(a)\
    (a) >= 0 ? (a) : -(a)
    
#define get(list, i, j, default) \
    (i >= 0 && i < (list)->h && j >= 0 && j < (list)->w) ? (list)->at[i][j] : default
    
int min_top_triangle(energy* e, int i, int j)
{
    double left = get(e, i - 1, j - 1, __DBL_MAX__);
    double top = get(e, i - 1, j, __DBL_MAX__);
    double right = get(e, i - 1, j + 1, __DBL_MAX__);

    if (top == __DBL_MAX__) printf("out of the image\n");

    if (left <= top && left <= right)   return j - 1;
    if (top <= left && top <= right)    return j;
    if (right <= left && right <= top)  return j + 1; 
    
    printf("min_top_triangle pas normal\n");
    return -1;
    // return min(min(left, top), min(right, top));
}

int path_min_index(energy* e, int i)
{
    int j_min = 0;
    int e_min = e->at[i][j_min];
    for (int j = 0; j < e->w; ++j) {
        if (e_min > e->at[i][j]) {
            e_min = e->at[i][j];
            j_min = j;
        }
    }
    return j_min;
}

void reduce_pixels(image* im, int n);

int main(int argc, char* argv[])
{
    if (argc < 3) {
        printf("Fournir le fichier d'entrée et de sortie.\n");
        exit(EXIT_FAILURE);
    }
    // declaring the values
    char* f_in = argv[1];
    char* f_out = argv[2];
    image* im = image_load(f_in);
    energy* e = energy_new(im->h, im->w);
    path* p = path_new(e->h);

    // Do some processing here
    // compute_energy(im, e);
    // energy_min_path(e);
    // compute_min_path(e, p);
    // 
    // int line = 0;
    // printf("\033[34;1;4m%3d\033[30;1;4m - \033[32;1;4m%-3d\033[0m: ", line, p->at[line]);
    // int start = max(p->at[line] - 1, 0);
    // int end = min(p->at[line] + 1, e->w - 1);
    // for (int j = start; j <= end; ++j) {
    //     if (j == p->at[line]) printf("\033[31;1;4m");
    //     printf("%7.2f", e->at[line][j]);
    //     if (j == p->at[line]) printf("\033[0m, ");
    //     else printf(", ");
    // }printf("\n\n");
    // printf("\033[34;1;4m%3d\033[30;1;4m - \033[32;1;4m%-3d\033[0m: ", line, p->at[line]);
    // for (int j = 0; j < e->w; ++j) {
    //     if (j % 10 == 0) printf("\n%-3d|", j);
    //     printf("%7.2f, ", e->at[line][j]);
    // }printf("\n\n\n");
    //
    // for (int i = 10; i >= 0; --i) {
    //     int start = max(p->at[i] - 1, 0);
    //     int end = min(p->at[i] + 1, e->w - 1);
    //     printf("\033[34;1;4m%3d\033[30;1;4m - \033[32;1;4m%-3d\033[0m: ", i, p->at[i]);
    //     for (int j = start; j <= end; ++j) {
    //         if (j == p->at[i]) printf("\033[31;1;4m");
    //         printf("%7.2f", e->at[i][j]);
    //         if (j == p->at[i]) printf("\033[0m, ");
    //         else printf(", ");
    //     }
    //     printf("\n");
    // }
    // printf("\n");

    reduce_seam_carving(im, 150);

    // Saving the image    
    image_save(im, f_out);

    // freeing the values
    path_free(p);
    energy_free(e);
    image_free(im);
    return 0;
}

void reduce_seam_carving(image* im, int n)
{
    if (n <= 0) return;
    energy* e = energy_new(im->h, im->w);
    path* p = path_new(e->h);
    
    for (int k = 0; k < n; ++k) {
    compute_energy(im, e);
    energy_min_path(e);
    compute_min_path(e, p);
    for (int i = 0; i < im->h; ++i) {
        for (int j = p->at[i]; j < im->w - 1; ++j) {
            im->at[i][j] = im->at[i][j + 1];
        }
    }
    --(im->w);
    }

    energy_free(e);
    path_free(p);
}

void compute_min_path(energy* e, path* p)
{
    // NOTE: indice de l'énergie minimum de la dernière colonne
    int j = path_min_index(e, e->h - 1);
    p->at[e->h - 1] = j;

    // NOTE: de l'antépénultième à la deuxième
    for (int i = e->h - 1; i > 0; --i) {
        j = min_top_triangle(e, i, j);
        p->at[i - 1] = j;
    }
}

/*
Transformation en matrice utilisable par l'algo de Moore-Dijkstra.
*/
void energy_min_path(energy* e)
{
    // la colonne 1 ne change pas
    for (int i = 1; i < e->h; ++i) {
        for (int j = 0; j < e->w; ++j) {
            int j_min = min_top_triangle(e, i, j);

            e->at[i][j] += e->at[i-1][j_min];
        }
    }
}

void path_free(path* p)
{
    if (p == NULL) return;
    free(p->at);
    free(p);
    p = NULL;
}

path* path_new(int n)
{
    path* p = calloc(1, sizeof(path));
    assert(p != NULL && "Buy more RAM lol");
    p->size = n;
    p->at = calloc(n, sizeof(int));
    assert(p->at != NULL && "Buy more RAM lol");
    return p;
}

void reduce_column(image* im, int n)
{
    energy* e = energy_new(im->h, im->w);
    for (int i = 0; i < n; ++i) {
        compute_energy(im, e);
        reduce_one_column(im, e);
    }
    energy_free(e);
}

void reduce_one_column(image* im, energy* e)
{
    int j_start = best_column(e);
    for (int i = 0; i < e->h; ++i) {
        for (int j = j_start; j < e->w - 1; ++j) {
            im->at[i][j] = im->at[i][j + 1];
            e->at[i][j] = e->at[i][j + 1];
        }
    }
    --(im->w);
    --(e->w);
}

double sum_line(double* line, int w) {
    double r = 0;
    for (int i = 0; i < w; ++i) {
        r += line[i];
    }
    return r;
}

int best_column(energy* e)
{
    int i_min = 0;
    double temp, e_min = sum_line(e->at[0], e->w);
    for (int i = 1; i < e->h; ++i) {
        temp = sum_line(e->at[i], e->w);
        if (e_min > temp) {
            i_min = i;
            e_min = temp;
        }
    }
    return i_min;
}

void reduce_pixels(image* im, int n)
{
    energy* e = energy_new(im->h, im->w);
    for (int i = 0; i < n; ++i) {
        compute_energy(im, e);
        reduce_one_pixel(im, e);
        --(im->w);
        --(e->w);
    }
    energy_free(e);
    // NOTE: Les pixels se décalent.
}

void reduce_one_pixel(image* im, energy* e)
{
    for (int i = 0; i < im->h; ++i) {
        remove_pixel(im->at[i], e->at[i], im->w);
    }
}

void remove_pixel(uint8_t* line, double* e, int w)
{
    assert(w > 0);
    int i_min = 0;
    double e_min = e[0];
    for (int i = 1; i < w; ++i) {
        if (e_min > e[i]) {
            e_min = e[i];
            i_min = i;
        }
    }
    for (int i = i_min; i < w - 1; ++i) {
        line[i] = line[i + 1];
        e[i] = e[i + 1];
    }
}

image* energy_to_image(energy* e)
{
    image* img = image_new(e->h, e->w);
    for (int i = 0; i < img->h; ++i) {
        for (int j = 0; j < img->w; ++j) {
            if (e->at[i][j] > 0)
                img->at[i][j] = 0x00;
            else img->at[i][j] = 0xFF;
        }
    }
    return img;
}

void compute_energy(image* im, energy* e)
{
    for (int i = 0; i < e->h; ++i) {
        for (int j = 0; j < e->w; ++j) {
            int i_b = min(i + 1, im->h - 1);
            int j_r = min(j + 1, im->w - 1);
            int i_t = max(i - 1, 0);
            int j_t = max(j - 1, 0);
            
            e->at[i][j] = (abs(im->at[i][j_r] - im->at[i][j_t]) / (j_r - j_t)) 
            + (abs(im->at[i_b][j] - im->at[i_t][j]) / (i_b - i_t));
        }
    }
}

void energy_free(energy* e)
{
    if (e == NULL) return;
    for (int i = 0; i < e->h; ++i) {
        free(e->at[i]);
    }
    free(e->at);
    free(e);
    e = NULL;
}

energy* energy_new(int h, int w)
{
    energy* ene = calloc(1, sizeof(energy));
    assert(ene != NULL && "Buy more RAM lol!");
    ene->h = h;
    ene->w = w;

    ene->at = calloc(h, sizeof(double*));
    assert(ene->at != NULL && "Buy more RAM lol!");
    for (int i = 0; i < ene->h; ++i) {
        ene->at[i] = calloc(w, sizeof(double));
        assert(ene->at[i] != NULL && "Buy more RAM lol!");
    }
    return ene;
}

void flip_horizontal(image* im)
{
    for (int i = 0; i < im->h; ++i) {
        for (int j = 0; j <= (im->w - 1) / 2; ++j) {
            uint8_t temp = im->at[i][j];
            im->at[i][j] = im->at[i][im->w - 1 - j];
            im->at[i][im->w - 1 - j] = temp;
        }
    }
}

void binarize(image* im)
{
    for (int i = 0; i < im->h; ++i) {
        for (int j = 0; j < im->w; ++j) {
            if (im->at[i][j] <= 0x7F) // 0xFF / 2 = 0x7F
                im->at[i][j] = 0x00;
            else
                im->at[i][j] = 0xFF;
        }
    }
}

void invert(image* im)
{
    for (int i = 0; i < im->h; ++i) {
        for (int j = 0; j < im->w; ++j) {
            im->at[i][j] = 0xFF - im->at[i][j];
        }
    }
}

void image_free(image* im)
{
    if (im == NULL) return;
    for (int i = 0; i < im->h; ++i) {
        free(im->at[i]);
    }
    free(im->at);
    free(im);
    im = NULL;
}

image* image_new(int h, int w)
{
    image* img = calloc(1, sizeof(image));
    assert(img != NULL && "Buy more RAM lol!");
    img->h = h;
    img->w = w;

    img->at = calloc(h, sizeof(uint8_t*));
    assert(img->at != NULL && "Buy more RAM lol!");
    for (int i = 0; i < img->h; ++i) {
        img->at[i] = calloc(w, sizeof(uint8_t));
        assert(img->at[i] != NULL && "Buy more RAM lol!");
    }
    return img;
}

image* image_load(char* filename) {
    int w, h, channels;
    uint8_t* data = stbi_load(filename, &w, &h, &channels, 0);
    if (!data) {
        fprintf(stderr, "Erreur de lecture.\n");
        stbi_failure_reason();
        exit(EXIT_FAILURE);
    }
    if (channels != 1) {
        fprintf(stdout, "Pas une image en niveaux de gris.\n");
        exit(EXIT_FAILURE);
    }
    image* im = image_new(h, w);
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            im->at[i][j] = data[j + i * w];
        }
    }
    free(data);
    return im;
}

void image_save(image* im, char* filename) {
    int h = im->h;
    int w = im->w;
    int stride_length = w;
    uint8_t* data = malloc(w * h * sizeof(uint8_t));
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            data[j + i * w] = im->at[i][j];
        }
    }
    if (!stbi_write_png(filename, w, h, 1, data, stride_length)) {
        fprintf(stderr, "Erreur d'écriture.\n");
        image_free(im);
        free(data);
        exit(EXIT_FAILURE);
    }
    free(data);
}
