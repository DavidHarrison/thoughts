/*
 * file: GenHashtable.h
 */

#ifndef GEN_HASHTABLE
#define GEN_HASHTABLE

typedef struct {
    HT_KEY_TYPE key;
    HT_VAL_TYPE val;
} item;

#include "GenList.h"

#define LIST_TYPE item*

#define HT_SIZE 100

typedef list* hashtable;

int main();
hashtable new();
void add(HT_KEY_TYPE key, HT_VAL_TYPE val, hashtable ht);
int exists(HT_KEY_TYPE key, hashtable ht);
HT_VAL_TYPE *lookup(HT_KEY_TYPE key, hashtable ht);
void freeHt(hashtable ht);

#endif
