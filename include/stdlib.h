#pragma once

#include "types.h"

void *malloc(__size_t size);
void *calloc(__size_t nmemb, __size_t size);
void *realloc(void *ptr, __size_t size);
void free(void *ptr);

void qsort(void *base, __size_t nitems, __size_t size,
           int (*compar)(const void *, const void *));

void abort(void);
