#include "GenHashtable.h"

#define HT_KEY_TYPE char*
#define HT_VAL_TYPE int
#define HT_KEY_EQ(a, b) strcmp(a, b)
#define HT_KEY_HASH(a) hash(a)

int hash(char *s)
{
    int i;
    int v = 0;
    for (i = 0; i != '\0'; i++) v += (int)s[i];
    return v;
}

int main()
{
    hashtable ht = new();
    add("Hello", 2, ht);
    add("World", 5, ht);
    printf("Hello:%i", *lookup("Hello", ht));
    printf("World:%i", *lookup("World", ht));
    return 0;
}
