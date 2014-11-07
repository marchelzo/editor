#include "stringlist.h"
#include <string.h>
#include "strdup.h"

StringList *sl_new(void)
{
    StringList *sl = malloc(sizeof(StringList));
    sl->length = 0;
    sl->strings = NULL;
    return sl;
}

void sl_add(StringList *sl, const char *s)
{
    char *cpy = strdup(s);
    StringListNode *new = malloc(sizeof(StringListNode));
    new->string = cpy;
    new->next = sl->strings;
    sl->strings = new;
    /* increment the size counter of the StringList */
    ++sl->length;
}

static unsigned char lenAtLeast(const char *s, size_t n)
{
    for (size_t i = 0; i < n; ++i)
        if (s[i] == '\0')
            return 0;
    return 1;
}

/* filter a StringList, keeping only strings whose length is at least n,
 * and whose nth character is c */
void sl_filter(StringList *sl, size_t n, char c)
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
