#include "GenHashtable.h"

hashtable new()
{
    return calloc(HT_SIZE, sizeof(item*));
}

#define LOC (ht + HT_KEY_HASH(key))

void add(HT_KEY_TYPE key, HT_VAL_TYPE val, hashtable ht)
{
    item *new_item = malloc(sizeof(item));
    new_item->key = key;
    new_item->val = val;
    *LOC = cons(*LOC, new_item);
}

int exists(HT_KEY_TYPE key, hashtable ht)
{
    if (lookup(key, ht) == NULL) return 0;
    return 1;
}

HT_VAL_TYPE *lookup(HT_KEY_TYPE key, hashtable ht)
{
    node *p;
    for (p = *LOC; p != NULL; p = p->next)
    {
        if (p->val->key == key) return &(p->val->val);
    }
    return NULL;
}

void freeHt(hashtable ht)
{
    int i;
    for (i = 0; i < HT_SIZE; i++)
    {
        freeList(*(ht + i));
    }
    free(ht);
}
