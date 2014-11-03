/* Global variables that describe the state (mode, buffers, etc.) */

#ifndef __STATE_H__
#define __STATE_H__

#include "mode.h"
#include "buffer.h"

/* active Editor Mode global */
extern EditorMode g_mode;

/* current buffer global */
extern Buffer *g_cb;

/* global input handler */
extern void (*g_handleInput)(int);

/* global message for debugging */
extern char *g_msg;

#endif
