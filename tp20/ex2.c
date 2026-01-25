#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>

typedef struct {
    int* data;
    size_t size;
    size_t capacity;
} queue_t;

queue_t* new_q(size_t capacity);
void free_q(queue_t* q);
void display_q(queue_t* q, bool all);
void push(queue_t* q, int value);
int pop(queue_t* q);
int peek(queue_t* q);

int main(void) {
    queue_t* q = new_q(9);

    // push
    printf("push: ");
    for (int i = 0; i < 18; ++i) {
        push(q, i);
        printf("%d, ", peek(q));
    }
    printf("\n");
    display_q(q, true);

    // peek
    printf("peek: %d\n", peek(q));
    display_q(q, false);
    
    // pop
    printf("pop: ");
    for (int i = 0; i < 1; ++i) {
        int v = pop(q);
        printf("%d ", v);
    }
    printf("\n");
    display_q(q, false);

    free_q(q);
    return 0;
}

int peek(queue_t* q) {
    int v = pop(q);
    push(q, v);
    return v;
}

int pop(queue_t* q) {
    if (q->size > 0) {
        q->size = q->size - 1;
    } else {
        q->size = q->capacity - 1;
    }

    return q->data[q->size];
}

void push(queue_t* q, int value) {
    q->data[q->size] = value;
    q->size = (q->size + 1) % q->capacity;
}

void display_q(queue_t* q, bool all) {
    size_t stop = 0;
    if (all) stop = q->capacity;
    else stop = q->size;

    printf("|%2zu: [", q->size);
    for (size_t i = 0; i < stop; ++i) {
        printf("%d, ", q->data[i]);
    }
    printf("]\n");
}

void free_q(queue_t* q) {
    assert(q != NULL);
    free(q->data);
    free(q);
}

queue_t* new_q(size_t capacity) {
    queue_t* q = calloc(1, sizeof(queue_t));
    assert(q != NULL && "Buy more RAM lol");

    /*
    NOTE: If size is 0, then malloc() returns a unique pointer
        value that can later be successfully passed to free().
    NOTE: For our uses though, capacity must be non null because of how push is
        implemented.
    */
    q->capacity = capacity;
    q->data = calloc(q->capacity, sizeof(int));
    assert(q->data != NULL && "Buy more RAM lol");

    q->size = 0;
    return q;
}
