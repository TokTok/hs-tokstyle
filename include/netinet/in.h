#pragma once

#include "../types.h"

typedef __uint32_t in_addr_t;

enum {
  IPPROTO_TCP = 6,   /* Transmission Control Protocol    */
  IPPROTO_UDP = 17,  /* User Datagram Protocol           */
  IPPROTO_IPV6 = 41, /* IPv6-in-IPv4 tunnelling          */
};

struct in_addr {
  __uint32_t s_addr;
};
struct in6_addr {
  __uint8_t s6_addr[16];
};

extern const struct in6_addr in6addr_loopback; /* ::1 */

#define INADDR_BROADCAST ((in_addr_t)0xffffffff)

enum {
  IPV6_ADD_MEMBERSHIP = 20,
  IPV6_V6ONLY = 26,
};

struct sockaddr_in {
  __sa_family_t sin_family;
  __uint16_t sin_port;
  struct in_addr sin_addr;
};

struct sockaddr_in6 {
  __sa_family_t sin6_family;
  __uint16_t sin6_port;
  struct in6_addr sin6_addr;
  __uint32_t sin6_flowinfo;
  __uint32_t sin6_scope_id;
};

struct sockaddr_storage {
  union {
    __sa_family_t ss_family;
    struct sockaddr_in __in;
    struct sockaddr_in6 __in6;
  };
};

struct ipv6_mreq {
  struct in6_addr ipv6mr_multiaddr;
  unsigned int ipv6mr_interface;
};

#define INET6_ADDRSTRLEN 66
#define INET_ADDRSTRLEN 22
