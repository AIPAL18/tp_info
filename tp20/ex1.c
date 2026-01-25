#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define STACK_MAX_LENGTH 1000

typedef struct {
    int* data;
    int size;
} stack_t;

stack_t* new_stack() {
    stack_t* s = calloc(1, sizeof(stack_t));
    s->data = calloc(1, STACK_MAX_LENGTH * sizeof(int));
    s->size = 0;
    return  s;
}

bool is_empty(stack_t* s) {
    return  s->size == 0;
}

bool is_full(stack_t* s) {
    return s->size == STACK_MAX_LENGTH;
}

void push(int x, stack_t* s) {
    assert(!is_full(s));
    s->data[s->size] = x;
    s->size++;
}

int pop(stack_t* s) {
    assert(!is_empty(s));
    s->size--;
    return s->data[s->size];
}

void free_stack(stack_t* s) {
    free(s->data);
    free(s);
    s = NULL;
}

void d_stack(stack_t* s);

int peek(stack_t* p) {
    int v = pop(p);
    push(v, p);
    return v;
}

bool egal(stack_t* p1, stack_t* p2) {
    bool r = true;
    stack_t* t1 = new_stack();
    stack_t* t2 = new_stack();
    int i = 0;
    while (!(is_empty(p1) || is_empty(p2))) {
        i++;
        int v1 = pop(p1);
        int v2 = pop(p2);
        push(v1, t1);
        push(v2, t2);
        if (v1 != v2) {
            r = false;
            break;
        }
    }
    printf("%c\n", (r ? 't' : 'f'));
    r = r || !is_empty(p1) || !is_empty(p2);

    while (!is_empty(t1)) {
        push(pop(t1), p1);
        push(pop(t2), p2);
    }
    
    free_stack(t1);
    free_stack(t2);
    return r;
}

int main(void) {
    stack_t* s = new_stack();
    int v;
    d_stack(s);

    push(10, s);
    push(20, s);
    d_stack(s);
    printf("\n");

    v = pop(s);
    printf("pop: %d\n", v);
    d_stack(s);
    printf("\n");

    v = peek(s);
    printf("peek: %d\n", v);
    d_stack(s);
    printf("\n");
    
    stack_t* s2 = new_stack();
    push(11, s2);

    printf("egal: %c\n", (egal(s, s2) ? 't' : 'f'));
    d_stack(s);
    d_stack(s2);
    
    free_stack(s);
    free_stack(s2);
    return  0;
}

void d_stack(stack_t* s) {
    printf("[");
    for (int i = 0; i < s->size; ++i) {
        printf("%d, ", s->data[i]);
    }
    printf("]\n");
}

