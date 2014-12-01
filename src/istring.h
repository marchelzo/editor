#ifndef __ISTRING_H__
#define __ISTRING_H__

#include <stdlib.h>

int istrcmp(const int *s1, const int *s2);
size_t istrlen(const int *s);
int *ifromstr(const char *s);
char *itostr(const int *s);

#endif
