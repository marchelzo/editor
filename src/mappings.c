#include "stringlist.h"
#include <string.h>
#include <ncurses.h>

#define RIGHT 261
#define LEFT 260
#define UP 259
#define DOWN 258

/* the number of characters entered so far */
static size_t charIndex = 0;

/* this list is the list which will be continuously filtered
 * as user input is parsed, and at the end of each complete
 * command, it is reset to a copy of one of the three other
 * mapping lists.
 */
static StringList *currentList = NULL;

/* mappings for each editor mode */
static StringList *insertMappings = NULL;
static StringList *normalMappings = NULL;
static StringList *visualMappings = NULL;
static StringList *commandMappings = NULL;

struct entity {
    int replacement;
    const char *s;
};

static unsigned char isPrefix(const char *pre, const char *s)
{
    size_t prefixLength = strlen(pre);
    if (strlen(s) < prefixLength) return 0;
    for (size_t i = 0; i < prefixLength; ++i)
        if (pre[i] != s[i]) return 0;
    return 1;
}

static const size_t numEntities = 9;
static struct entity entities[] = { { .replacement = 13,      .s = "<CR>"    }
                                  , { .replacement = '\t',    .s = "<Tab>"   }
                                  , { .replacement = ' ',     .s = "<Space>" }
                                  , { .replacement = KEY_BACKSPACE, .s = "<BS>"    }
                                  , { .replacement = LEFT,    .s = "<Left>"  }
                                  , { .replacement = RIGHT,   .s = "<Right>" }
                                  , { .replacement = UP,      .s = "<Up>"    }
                                  , { .replacement = DOWN,    .s = "<Down>"  }
                                  , { .replacement = 0,       .s = "<C-"     }
                                  };

int *expandEntities(const char *str)
{
    size_t len = strlen(str);
    size_t i = 0;
    int *istr = malloc(sizeof(int) * (len + 1));
    char *s = (char *)str;
    while (*s) {
        while (*s != '<') {
            if (!*s) goto ret;
            istr[i++] = *s++;
        }
        for (size_t j = 0; j < numEntities; ++j) {
            if (isPrefix(entities[j].s, s)) {
                if (entities[j].replacement == 0) {
                    /* special case, when we match "<C-" */
                    while (*s != '>' && *s) ++s;
                    istr[i++] = s[-1] - 96;
                    if (*s == '>') ++s;
                    goto next;
                }
                istr[i++] = entities[j].replacement;
                while (*s != '>' && *s) ++s;
                if (*s == '>') ++s;
                goto next;
            }
        }
        istr[i++] = *s++;
next:
        ;
    }
ret:
    istr[i] = 0;
    return istr;
}

/* Implementation of the Mapping interface */

void mappings_setNormal(void)
{
    sl_free(currentList);
    currentList = sl_copy(normalMappings);
    charIndex = 0;
}

void mappings_setInsert(void)
{
    sl_free(currentList);
    currentList = sl_copy(insertMappings);
    charIndex = 0;
}

void mappings_setCommand(void)
{
    sl_free(currentList);
    currentList = sl_copy(commandMappings);
    charIndex = 0;
}

void mappings_setVisual(void)
{
    sl_free(currentList);
    currentList = sl_copy(visualMappings);
    charIndex = 0;
}

void mappings_newNormal(const char *to, const char *from)
{
    sl_add(normalMappings, expandEntities(to), expandEntities(from));
}

void mappings_newInsert(const char *to, const char *from)
{
    sl_add(insertMappings, expandEntities(to), expandEntities(from));
}

void mappings_newVisual(const char *to, const char *from)
{
    sl_add(visualMappings, expandEntities(to), expandEntities(from));
}

void mappings_newCommand(const char *to, const char *from)
{
    sl_add(commandMappings, expandEntities(to), expandEntities(from));
}

const StringList *mappings_getPossibilities(void)
{
    return currentList;
}

void mappings_filter(int c)
{
    sl_filter(currentList, charIndex++, c);
}

void mappings_init(void)
{
    normalMappings = sl_new();
    visualMappings = sl_new();
    insertMappings = sl_new();
    commandMappings = sl_new();
}
