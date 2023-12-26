#pragma once

#include "types.h"

typedef __int8_t int8_t;
typedef __uint8_t uint8_t;
typedef __int16_t int16_t;
typedef __uint16_t uint16_t;
typedef __int32_t int32_t;
typedef __uint32_t uint32_t;
typedef __int64_t int64_t;
typedef __uint64_t uint64_t;

typedef unsigned int uint_fast16_t;

extern const uint8_t UINT8_MAX;
extern const int8_t INT8_MAX;
extern const uint16_t UINT16_MAX;
extern const int16_t INT16_MAX;
extern const uint32_t UINT32_MAX;
extern const int32_t INT32_MAX;
extern const uint64_t UINT64_MAX;
extern const int64_t INT64_MAX;

extern uint32_t UINT32_C(int value);
extern uint64_t UINT64_C(int value);
