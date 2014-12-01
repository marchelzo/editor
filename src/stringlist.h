#ifndef __STRINGLIST_H__
#define __STRINGLIST_H__

/* StringList is for gradually filtering a list
 * of strings. e.g. If you want all strings beginning
 * with the letter 'a', and there are 80, and then you
 * assert than the second letter must be 'b', you
 * can filter the previous results without checking
 * every string all over again.
 * 
 * StringList does not create copies of strings, but
 * rather maintains pointers to them. Thus, you should
 * not mutate any strings stored in a StringList, unless
 * you are aware of the consequences
 */

#include <stdlib.h>

#ifdef SL_DEBUG
#include <stdio.h>
#endif

typedef struct sl_node {
    struct sl_node *next;
    const int *string;
    const int *mapValue;
} StringListNode; 

typedef struct {
    StringListNode *strings;
    size_t length;
} StringList;

StringList *sl_new(void);

StringList *sl_copy(StringList *other);

void sl_add(StringList *sl, const int *s, const int *mapValue);

void sl_filter(StringList *sl, size_t n, int c);

const int *sl_contains(const StringList *sl, const int *needle);

void sl_free(StringList *sl);


#ifdef SL_DEBUG
void sl_print(StringList *sl, FILE *f);
#endif

#endif
