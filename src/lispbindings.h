#include "state.h"
#include "editbuffer.h"

void evalLisp(char *);

void next_buffer(void);
void prev_buffer(void);
void new_buffer(char *);
void normal_eval(char *);
void eval_buffer(void);
void normal_map(char *, char *);
void insert_map(char *, char *);
void visual_map(char *, char *);
void command_map(char *, char *);
