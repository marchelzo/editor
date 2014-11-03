#include "quit.h"

void g_edit_quit(void)
{
    clear();
    endwin();
    curs_set(1);
    exit(0);
}
