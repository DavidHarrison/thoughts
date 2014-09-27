#ifndef GEN_LIST
#define GEN_LIST

#include <stdlib.h>
#include <stdio.h>

#include "GenHashtable.h"

typedef struct _node {
    LIST_TYPE val;
    struct _node *next;
} node;

typedef node* list;

void append(list first, list second);
list cons(list l, LIST_TYPE val);
void snoc(list l, LIST_TYPE val);
LIST_TYPE head(list l);
list tail(list l);
list init(list l);
LIST_TYPE last(list l);
list reverse(list l);
void freeList(list l);

#endif
