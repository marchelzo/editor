#ifndef __COMMANDCOMPLETION_H__
#define __COMMANDCOMPLETION_H__

#include "state.h"
#include "stringlist.h"

typedef struct {
    StringList *completions;
    unsigned char currentLetter;
} CompletionList;

CompletionList *compl_newAll(char c);

void compl_filterCompletions(CompletionList *cl, const char *com);

void compl_addGlobalCommand(char *s);

void compl_initializeGlobalCommandList(void);

#endif
