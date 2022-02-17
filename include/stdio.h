#pragma once

#include "types.h"

typedef struct FILE FILE;

extern FILE *stdout;
extern FILE *stderr;

int fputc(char c, FILE *file);
int fprintf(FILE *file, const char *fmt, ...);
int snprintf(char *buf, __size_t buflen, const char *fmt, ...);
int vsnprintf(char *buf, __size_t buflen, const char *fmt,
              __builtin_va_list ap);
