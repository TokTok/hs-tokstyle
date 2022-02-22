#pragma once

struct addrinfo {
  int ai_family;
  int ai_socktype;
  struct sockaddr *ai_addr;
  int ai_addrlen;
  struct addrinfo *ai_next;
};

int getaddrinfo(const char *node, const char *service,
                const struct addrinfo *hints, struct addrinfo **res);

void freeaddrinfo(struct addrinfo *res);
