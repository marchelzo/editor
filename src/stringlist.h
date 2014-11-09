#ifndef __STRINGLIST_H__
#define __STRINGLIST_H__

#include <stdlib.h>

typedef struct sl_node {
    struct sl_node *next;
    char* string;
} StringListNode; 

typedef struct {
    StringListNode *strings;
    size_t length;
} StringList;

StringList *sl_new(void);

void sl_add(StringList *sl, const char *s);

void sl_filter(StringList *sl, size_t n, char c);

#endif
