#include "commandcompletion.h"
#include "state.h"

#include <string.h>

CompletionList *compl_newAll(char c)
{
    CompletionList *cl = malloc(sizeof(CompletionList));
    cl->currentLetter = 1;
    cl->completions = malloc(sizeof(StringList));
    cl->completions->length = 0;
    StringList *l = g_commandList + c - 'A';
    if (l->length == 0)
        return cl;
    StringListNode *gNode = l->strings;
    cl->completions->strings = malloc(sizeof(StringListNode));
    cl->completions->length = l->length;
    StringListNode *n = cl->completions->strings;
    for (size_t i = 1; i < l->length; ++i) {
        n->string = gNode->string;
        n->next = malloc(sizeof(StringListNode));
        n = n->next;
        gNode = gNode->next;
    }
    n->string = gNode->string;
    n->next = NULL;
    return cl;
}

void compl_filterCompletions(CompletionList *cl, const char *com)
{
    if (com == NULL)
        return;

    size_t len = strlen(com);
    while (cl->currentLetter < len) {
        sl_filter(cl->completions, cl->currentLetter, com[cl->currentLetter]);
        ++cl->currentLetter;
    }
}

static inline unsigned char letter(char c)
{
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

void compl_addGlobalCommand(char *com)
{
    if (com == NULL)
        return;
    if (!(letter(com[0])))
        return;
    sl_add(g_commandList + (com[0] - 'A'), com);
}

void compl_initializeGlobalCommandList(void)
{
    g_commandList = malloc(60 * sizeof(StringList));
}
