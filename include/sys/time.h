#pragma once

#include "../time.h"

typedef enum {
  CLOCK_MONOTONIC = 0,
} clockid_t;

typedef long suseconds_t;

struct timeval {
  time_t tv_sec;
  suseconds_t tv_usec;
};

struct timespec {
  time_t tv_sec;
  long tv_nsec;
};

int clock_gettime(clockid_t clk_id, struct timespec *tp);
