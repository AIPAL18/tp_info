#ifndef CHECK_ALLOC_H_
#define CHECK_ALLOC_H_
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#ifdef CHECK_ALLOC_VERBOSE
#   define LOG(format, ...) printf(format, __VA_ARGS__)
#else
#   define LOG(format, ...) ((void)0)
#endif // CHECK_ALLOC_VERBOSE

typedef struct Alloc {
    void *ptr;
    size_t size;
    struct Alloc *next;
} Alloc;

static Alloc *alloc_list;

void* my_malloc(size_t size);

void* my_calloc(size_t number, size_t size);

void* my_realloc(void* pointer, size_t size);

void my_free(void *ptr);

void check_leaks();

#ifdef CHECK_ALLOC_IMPLEMENTATION

bool memory_insufficient = false;

static Alloc *alloc_list = NULL;

void* my_malloc(size_t size) {
    void *ptr = malloc(size);
    LOG("(ALLOC SCAN) +%zu\n", size);
    if (ptr) {
        Alloc *a = malloc(sizeof(Alloc));
        if (a) {
            a->ptr = ptr;
            a->size = size;
            a->next = alloc_list;
            alloc_list = a;
        } else memory_insufficient = true;
    }
    return ptr;
}

void* my_calloc(size_t number, size_t size) {
    void *ptr = calloc(number, size);
    LOG("(ALLOC SCAN) +%zu\n", size * number);
    if (ptr) {
        Alloc *a = malloc(sizeof(Alloc));
        if (a) {
            a->ptr = ptr;
            a->size = size * number;
            a->next = alloc_list;
            alloc_list = a;
        } else memory_insufficient = true;
    }
    return ptr;
}

void* my_realloc(void* pointer, size_t size) { // Chat-GPT
    if (pointer == NULL) {
        return my_malloc(size);
    }
    if (size == 0) {
        my_free(pointer);
        return NULL;
    }

    Alloc *curr = alloc_list;
    while (curr) {
        if (curr->ptr == pointer) {
            break;
        }
        curr = curr->next;
    }

    if (!curr) {
        // Pointeur inconnu -> comportement indéfini, on peut ignorer ou log
        LOG("(ALLOC SCAN) Warning: realloc on unknown pointer %p\n", pointer);
        return realloc(pointer, size); // fallback standard
    }

    void *new_ptr = realloc(pointer, size);
    if (!new_ptr) {
        // realloc a échoué, l'ancien bloc est toujours valide
        return NULL;
    }

    LOG("(ALLOC SCAN) %p: %zu -> %zu\n", pointer, curr->size, size);
    curr->ptr = new_ptr;
    curr->size = size;

    return new_ptr;
}

void my_free(void *ptr) {
    Alloc **curr = &alloc_list;
    while (*curr) {
        if ((*curr)->ptr == ptr) {
            Alloc *to_free = *curr;
            *curr = (*curr)->next;
            LOG("(ALLOC SCAN) -%zu\n", to_free->size);
            free(to_free);
            break;
        }
        curr = &(*curr)->next;
    }
    free(ptr);
}

void check_leaks() {
    if (memory_insufficient) {
        printf("(ALLOC SCAN) The leak check couldn't run properly: memory capacity is insufficient.\n");
        return;
    }
    Alloc *curr = alloc_list;
    while (curr) {
        printf("(ALLOC SCAN) Leak detected: %p (%zu bytes)\n", curr->ptr, curr->size);
        my_free(curr->ptr);
        curr = curr->next;
    }
    printf("(ALLOC SCAN) No other leak detected.\n");
}

#endif // CHECK_ALLOC_IMPLEMENTATION

#endif // CHECK_ALLOC_H_
