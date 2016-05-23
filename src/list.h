#ifndef LIST_H
#define LIST_H

typedef struct node_t {
        struct node_t *next;
        int val;
} node_t;

typedef struct list_t {
        struct node_t *first;
        struct node_t *last;
        size_t size;
} list_t;


list_t *list();
void append(list_t *l, int val);
void push(list_t *l, int val);
void erase(list_t *l);

#endif
