#pragma once

#include "types.h"

struct sockaddr {
  __sa_family_t sa_family;
};

struct ifreq {
  struct sockaddr ifr_broadaddr;
};

struct ifconf {
  char *ifc_buf;
  unsigned int ifc_len;
};

extern int SIOCGIFCONF;
extern int SIOCGIFBRDADDR;
