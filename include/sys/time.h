#pragma once

typedef enum {
    CLOCK_MONOTONIC = 0,
} clockid_t;

struct timespec {
    unsigned long tv_sec;
    unsigned long tv_nsec;
};

int clock_gettime(clockid_t clk_id, struct timespec *tp);
