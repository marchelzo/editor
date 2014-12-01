/* Global variables that describe the state (mode, buffers, etc.) */

#ifndef __STATE_H__
#define __STATE_H__

#include "mode.h"
#include "buffer.h"
#include "editbuffer.h"
#include "stringlist.h"
#include "hashmap.h"

/* current buffer global */
extern EditBuffer *g_cb;

/* current buffer list */
extern EditBuffer **g_bufList;

/* the number of EditBuffers currently in g_bufList */
extern int g_numBuffers;

/* global message for debugging */
extern char *g_msg;

/* last command run in command mode */
extern char *g_command;

/* terminal dimensions as interpreted by ncurses */
extern int g_termCols;
extern int g_termRows;

/* list of commands used for auto completion in command mode */
extern StringList *g_commandList;

/* hashmap that maps commands to actions */
extern HashMap *g_commandMap;

/* result of the last evaluated lisp expr */
extern char *g_evalResult;

/* StringList that contains all of the mappings for the current editor mode */
extern StringList *keyMappings;

#endif
