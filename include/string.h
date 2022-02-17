#pragma once

#include "types.h"

int memcmp(const void *a, const void *b, __size_t size);
__size_t memcpy(void *dst, const void *src, __size_t size);
__size_t memmove(void *dst, const void *src, __size_t size);
void memset(void *ptr, char value, __size_t size);

const char *strrchr(const char *haystack, int needle);
const char *strstr(const char *haystack, const char *needle);

const __size_t strlen(const char *str);
