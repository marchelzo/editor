#include "syntax_coloring.h"
#include "slre/slre.h"
#include "state.h"
#include "buffer.h"
#include "editbuffer.h"
#include "gapbuffer.h"

#include <ncurses.h>

static const char *separators = " ,.)([]{}%+-=*/;:";

static unsigned char is_separator(char c)
{
    for (char *s = separators; *s; ++s)
        if (*s == c) return 1;
    return 0;
}

static int word_color(char *w, size_t len)
{
    if (slre_match("case", w, len, NULL, 0, 0) > 0)
        return 2;
    else if (slre_match("(int)|(long)|(size_t)|(char)|(unsigned)|(void)", w, len, NULL, 0, 0) > 0)
        return 3;
    else return 1;
}

static void highlight_line(char *l, size_t len, int line_num)
{
    if (*l == '#') {
        chgat(-1, A_NORMAL, 4, NULL);
        return;
    }
    int colOffset = (g_cb->conf->lineNumbers) ? 5 : 0;
    int word_length = 1;
    int col = 0;
    char *word_begin = l;
    char *word_end = l;
    while (*word_end) {
        while (*word_end && !is_separator(*word_end++)) ++word_length;
        int color = word_color(word_begin, word_length);
        mvchgat(line_num, col + colOffset, word_length - 1, A_BOLD, color, NULL);
        col += word_length;
        word_length = 1;
        word_begin = word_end;
    }
}

void highlight(void)
{
    size_t initial_line = g_cb->b->currentLine;
    for (size_t i = 0; i < g_termRows - 2; ++i) {
        b_goToLine(g_cb->b, i + g_cb->yScroll);
        move(i,0);
        highlight_line(gb_cString(g_cb->b->line->content), gb_length(g_cb->b->line->content), i);
    }
    b_goToLine(g_cb->b, initial_line);
}
