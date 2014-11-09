#include "strdup.h"
#include <stdlib.h>
#include <string.h>

char *strdup(const char *s)
{
    char *new = malloc(strlen(s) + 1);
    if (new)
        strcpy(new, s);
    return new;
}
