#include "quit.h"

void g_edit_quit(void)
{
    clear();
    move(0,0);
    refresh();
    endwin();
    exit(0);
}
