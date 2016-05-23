#include <stdlib.h>
#include <stdio.h>

typedef struct node_t {
        struct node_t *next;
        int val;
} node_t;

typedef struct list_t {
        struct node_t *first;
        struct node_t *last;
        size_t size;
} list_t;


/*
 * Create new empty list
 */
list_t *list()
{
        list_t *out = malloc(sizeof(list_t));
        out->first = NULL;
        out->last = NULL;
        out->size = 0;
        return out;
}


/*
 * Append object to end of list
 */
void append(list_t *l, int val)
{
        node_t *n = malloc(sizeof(node_t));
        n->val = val;
        n->next = NULL;
        
        if (l->size == 0) {
                l->first = n;
                l->last = n;
        } else {
                l->last->next = n;
                l->last = n;
        }
        l->size++;
}

/*
 * Push object to head of list
 */
void push(list_t *l, int val)
{
        node_t *node = malloc(sizeof(node_t));
        node->val = val;
        node->next = NULL;
        
        if (l->size == 0) {
                l->first = node;
                l->last = node;
        } else {
                node->next = l->first;
                l->first = node;
        }
        l->size++;
}

/*
 * Free list from mem
 */
void erase(list_t *l)
{
        node_t *old;
        for (int i = 0; i < l->size; i++) {
                old = l->first;
                l->first = l->first->next;
                free(old);
        }
        free(l);
}


int main() 
{
        list_t *l = list();
        append(l, 0);
        append(l, 5);
        append(l, 2);
        append(l, 1000);

        node_t *current = l->first;
        for (int i = 0; i < l->size; i++) {
                printf("[%d]: %d\n", i, current->val);
                current = current->next;
        }

        erase(l);
}
