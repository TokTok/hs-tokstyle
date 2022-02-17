#pragma once

typedef signed long __ssize_t;
typedef unsigned long __size_t;
typedef unsigned long __socklen_t;

// (Possibly) 32 bit system: long long for 64 bit ints.
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short __int16_t;
typedef unsigned short __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
typedef signed long long __int64_t;
typedef unsigned long long __uint64_t;

typedef enum {
  __AF_UNSPEC = 0,
  __AF_INET = 2,
  __AF_INET6 = 10,
} __sa_family_t;
