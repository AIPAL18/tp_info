#include <stdio.h>
#include <stdlib.h>

#define max(a, b)\
    (((a) >= (b)) ? (a) : (b))

void /*int***/ get_triangle(char* filename)
{
    FILE* f = fopen(filename, "r");
    if (f == NULL) {
        printf("Couldn't open \"%s\" !", filename);
    }

    size_t nb_line = 0;
    size_t nb_el_max = 0;
    size_t nb_el = 0;
    char buf = ' ';
    while (buf != EOF) {
        buf = fgetc(f);
        if (buf == EOF) continue;
        if (buf == '\n') {
            ++nb_line;
            nb_el_max = max(nb_el_max, nb_el);
            nb_el = 0;
        }
        if (buf == ' ') ++nb_el;
        printf("%c", buf);
    }
    printf("\n%zu par %zu\n", nb_line + 1, nb_el_max);
    
    fclose(f);
    
}

int main(void)
{
    get_triangle("triangle1.txt");
    return 0;
}
