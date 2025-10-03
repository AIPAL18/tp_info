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
    
void reduce_pixels(image* im, int n);

int main(int argc, char* argv[])
{
    if (argc < 3) {
        printf("Fournir le fichier d'entrée et de sortie.\n");
        exit(EXIT_FAILURE);
    }
    char* f_in = argv[1];
    char* f_out = argv[2];
    image* im = image_load(f_in);

    // Do some processing here
    // binarize(im);
    reduce_column(im, 50);
    
    image_save(im, f_out);

    image_free(im);
    return 0;
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
    compute_energy(im, e);
    for (int i = 0; i < n; ++i) {
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
    compute_energy(im, e);
    for (int i = 0; i < n; ++i) {
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
