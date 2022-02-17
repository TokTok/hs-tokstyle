#pragma once

#include "../types.h"

typedef __socklen_t socklen_t;

#define AF_UNSPEC __AF_UNSPEC
#define AF_INET   __AF_INET
#define AF_INET6  __AF_INET6

enum {
  SOCK_STREAM = 1,
  SOCK_DGRAM = 2,
};

enum {
  SOL_SOCKET = 1,
};

enum {
  SO_REUSEADDR = 2,
  SO_BROADCAST = 6,
  SO_SNDBUF = 7,
  SO_RCVBUF = 8,
};

int getsockopt(int sockfd, int level, int optname, void *restrict optval,
               socklen_t *restrict optlen);
int setsockopt(int sockfd, int level, int optname, const void *optval,
               socklen_t optlen);

struct sockaddr {};

int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
int listen(int sockfd, int backlog);
int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
int socket(int domain, int type, int protocol);

__ssize_t recv(int sockfd, void *buf, __size_t len, int flags);
__ssize_t recvfrom(int sockfd, void *buf, __size_t len, int flags,
                   struct sockaddr *src_addr, socklen_t *addrlen);

__ssize_t send(int sockfd, const void *buf, __size_t len, int flags);
__ssize_t sendto(int sockfd, const void *buf, __size_t len, int flags,
                 const struct sockaddr *dest_addr, socklen_t addrlen);
