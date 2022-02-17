#pragma once

enum {
  F_SETFD = 2, /* set/clear close_on_exec */
  F_SETFL = 4, /* set file->f_flags */
};

int fcntl(int fd, int cmd, ... /* arg */);
