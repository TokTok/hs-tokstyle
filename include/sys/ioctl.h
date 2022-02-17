#pragma once

enum {
  FIONREAD = 0x541B,
};

int ioctl(int fd, unsigned long request, ...);
