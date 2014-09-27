#include "GenList.h"

/*
int main()
{
    node *list, *p;
    int i;
    char *s;
    for (i = 0; i < 100; i++)
    {
        s = malloc(2 * sizeof(char));
        sprintf(s, "%i", i);
        list = cons(list, s);
    }
    list = reverse(list);
    for (p = list; p != NULL; p = p->next)
    {
        printf("%s\n", head(p));
    }
    freeList(list);
    return 0;
}
*/

void append(list first, list second)
{
    node *p;
    for (p = first; p->next != NULL; p = p->next);
    p->next = second;
}

list cons(list l, LIST_TYPE val)
{
    node *new_node = malloc(sizeof(node));
    new_node->val = val;
    new_node->next = l;
    return new_node;
}

void snoc(list l, LIST_TYPE val)
{
    node *new_node = malloc(sizeof(node));
    new_node->val = val;
    new_node->next = NULL;
    append(l, new_node);
}

LIST_TYPE head(list l)
{
    LIST_TYPE val = l->val;
    #ifdef AUTOFREE
    freeList(l);
    #endif
    return val;
}

list tail(list l)
{
    node *next = l->next;
    #ifdef AUTOFREE
    free(l);
    #endif
    return next;
}

list init(list l)
{
    node *p;
    for (p = l; p->next->next != NULL; p = p->next);
    #ifdef AUTOFREE
    free(p->next);
    #endif
    return l;
}

list end(list l)
{
    node *p;
    for (p = l; p->next != NULL; p = p->next);
    return p;
}

list reverse(list l)
{
    node *p, *next;
    node *new = NULL;
    for (p = l; p != NULL; p = next)
    {
        next = p->next;
        p->next = new;
        new = p;
    }
    return new;
}

void freeList(list l)
{
    node *p, *next;
    for (p = l; p->next != NULL; p = next)
    {
        next = p->next;
        free(p);
    }
}
