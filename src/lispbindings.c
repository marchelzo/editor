#include <HsFFI.h>
#include <curses.h>

#include "lispbindings.h"
#include "editbuffer.h"
#include "lisp/EditorLisp_stub.h"
#include "normal.h"
#include "mappings.h"
#include "gapbuffer.h"

void evalLisp(char *code)
{
  char *result = lispEval(code);
  free(g_evalResult);
  g_evalResult = result;
  refresh();
}

void next_buffer(void)
{
    if (g_cb->handle + 1 == g_numBuffers)
        return;
    g_cb = g_bufList[g_cb->handle + 1];
}

void prev_buffer(void)
{
    if (g_cb->handle == 0)
        return;
    g_cb = g_bufList[g_cb->handle - 1];
}

void new_buffer(char *s)
{
    EditBuffer *b = buf_new();
    buf_loadFile(b, s);
    g_cb = b;
}

void normal_eval(char *s)
{
    normalModeEval(s);
}

void eval_buffer(void)
{
    char *b = b_cString(g_cb->b);
    evalLisp(b);
    free(b);
}

void normal_map(char *to, char *from)
{
    mappings_newNormal(to, from);
    free(to);
    free(from);
}

void insert_map(char *to, char *from)
{
    mappings_newInsert(to, from);
    free(to);
    free(from);
}

void visual_map(char *to, char *from)
{
    mappings_newVisual(to, from);
    free(to);
    free(from);
}

void command_map(char *to, char *from)
{
    mappings_newCommand(to, from);
    free(to);
    free(from);
}

char get_char(void)
{
    return getch();
}

size_t line_number(void)
{
    return g_cb->b->currentLine;
}

char *current_line(void)
{
    return gb_cString(g_cb->b->line->content);
}

char *get_nth_line(size_t n)
{
    LineNode *node = g_cb->b->line;
    int k = n - g_cb->b->currentLine;
    if (k > 0)
        while (k-- > 0 && node->next)
            node = node->next;
    else if (k < 0)
        while (k++ < 0 && node->prev)
            node = node->prev;
    return gb_cString(node->content);
}
