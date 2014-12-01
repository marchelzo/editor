#include "istring.h"
#include <string.h>
#include <stdlib.h>

int istrcmp(const int *s1, const int *s2)
{
    while (1) {
        if (*s1 && *s2) {
            if (*s1 > *s2)
                return 1;
            else if (*s1 < *s2)
                return -1;
        } else {
            return (*s1 == *s2 ? 0 : (*s1 > *s2 ? 1 : -1));
        }
        ++s1;
        ++s2;
    }
}

size_t istrlen(const int *s)
{
    size_t len = 0;
    while (*s++ != 0) ++len;
    return len;
}

int *ifromstr(const char *s)
{
    size_t len = strlen(s);
    int *istr = malloc(sizeof(int) * (len + 1));
    for (size_t i = 0; i < len; ++i)
        istr[i] = s[i];
    istr[len] = 0;
    return istr;
}

char *itostr(const int *s)
{
    size_t len = istrlen(s);
    char *res = malloc(len);
    for (size_t i = 0; i <= len; ++i)
        res[i] = s[i];
    return res;
}
