#pragma once

#include "types.h"

enum {
    EAGAIN = 11,
    EWOULDBLOCK = EAGAIN,
};

extern int errno;

int strerror_r(int errnum, char *buf, __size_t buflen);
