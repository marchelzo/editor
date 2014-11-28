#include <HsFFI.h>
#include <curses.h>

#include "lispbindings.h"
#include "editbuffer.h"
#include "lisp/EditorLisp_stub.h"
#include "normal.h"

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
