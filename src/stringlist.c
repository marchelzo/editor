#include "stringlist.h"
#include <string.h>
#include "strdup.h"
#include "istring.h"

#ifdef SL_DEBUG
#include <stdio.h>
#endif

static void sl_removeIfExists(StringList *sl, const int *s)
{
    /* if the StringList is empty, we know it can't
     * contain the string `s` so we return immediately
     */
    if (!sl->strings)
        return;

    /* special case for checking if `s` matches the
     * first string in the StringList because in that
     * case, we need to mutate the StringList, and not
     * a StringListNode
     */
    if (istrcmp(sl->strings->string, s) == 0) {
        sl->strings = sl->strings->next;
        --sl->length;
        return;
    }

    StringListNode *n = sl->strings->next;
    StringListNode *prev = sl->strings;

    while (n) {
        if (istrcmp(n->string, s) == 0) {
            prev->next = n->next;
            --sl->length;
            return;
        }
        prev = n;
        n = n->next;
    }
}

StringList *sl_new(void)
{
    StringList *sl = malloc(sizeof(StringList));
    sl->length = 0;
    sl->strings = NULL;
    return sl;
}

StringList *sl_copy(StringList *other)
{
    StringList *new = sl_new();
    StringListNode *node = other->strings;

    for (size_t i = 0; i < other->length; ++i, node = node->next)
        sl_add(new, node->string, node->mapValue);

    return new;
}

void sl_add(StringList *sl, const int *s, const int *mapValue)
{
    sl_removeIfExists(sl, s);
    StringListNode *new = malloc(sizeof(StringListNode));
    new->string = s;
    new->mapValue = mapValue;
    new->next = sl->strings;
    sl->strings = new;
    /* increment the size counter of the StringList */
    ++sl->length;
}


static unsigned char lenAtLeast(const int *s, size_t n)
{
    for (size_t i = 0; i < n; ++i)
        if (s[i] == '\0')
            return 0;
    return 1;
}

/* filter a StringList, keeping only strings
 * whose length is at least n + 1,
 * and whose nth character is c */
void sl_filter(StringList *sl, size_t n, int c)
{
    StringListNode *node = sl->strings;
    StringListNode *prev = NULL;
    while (node) {
        if (lenAtLeast(node->string, n + 1) && node->string[n] == c) {
            /* this string is still valid, move on to check the next */
            prev = node;
            node = node->next;
        } else {
            /* remove the string from the list */
            if (prev == NULL) {
                sl->strings = node->next;
                node = node->next;
            } else {
                prev->next = node->next;
                node = node->next;
            }
            --sl->length;
        }
    }
}

const int *sl_contains(const StringList *sl, const int *needle)
{
    StringListNode *n = sl->strings;
    for (size_t i = 0; i < sl->length; ++i) {
        if (istrcmp(n->string, needle) == 0)
            return n->mapValue;
        n = n->next;
    }
    return NULL;
}

void sl_free(StringList *sl)
{
    if (!sl) return;

    StringListNode *n = sl->strings;
    StringListNode *temp = n;

    for (size_t i = 0; i < sl->length; ++i) {
        n = n->next;
        free(temp);
        temp = n;
    }
    free(sl);
}

#ifdef SL_DEBUG
void sl_print(StringList *sl, FILE *f)
{
    StringListNode *n = sl->strings;
    for (size_t i = 0; i < sl->length; ++i, n = n->next) {
        fprintf(f, "\"%s\": \"%s\"\n", n->string, n->mapValue);
    }
        
}
#endif
