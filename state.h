/* Global variables that describe the state (mode, buffers, etc.) */

#ifndef __STATE_H__
#define __STATE_H__

#include "mode.h"
#include "buffer.h"
#include "editbuffer.h"

/* current buffer global */
extern EditBuffer *g_cb;

/* global message for debugging */
extern char *g_msg;

/* last command run in command mode */
extern char *g_command;

/* terminal dimensions as interpreted by ncurses */
extern int g_termCols;
extern int g_termRows;

#endif
