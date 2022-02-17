#pragma once

#define assert(cond)                                                           \
  do {                                                                         \
    if (!(cond)) {                                                             \
      __assert_fail(#cond, __FILE__, __LINE__, __func__);                      \
    }                                                                          \
  } while (0)

void __assert_fail(const char *msg, const char *file, int line,
                   const char *func);
