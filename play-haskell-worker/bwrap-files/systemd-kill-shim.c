// This thing being setuid root is a bit iffy: starting units (what
// systemd-run-shim does) is one thing, but killing units is potentially
// harmful. However, we do have the barrier of defense that we only allow
// killing units here whose name starts with UNIT_NAME_PREFIX (see #define
// below); hopefully noone else names their units this way.

#define _GNU_SOURCE
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include "systemd-shim-inc.h"

int main(int argc, char **argv) {
  const uid_t parent_uid = getuid();
  const uid_t parent_gid = getgid();

  if (parent_uid == 0 || parent_gid == 0) {
    fprintf(stderr,
        "%s must not be run as root, it must be marked setuid root "
        "and run as a normal user.\n",
        argv[0]);
    return 1;
  }

  if (argc != 2) {
    fprintf(stderr,
        "Usage: %s <unit name>\n"
        "The unit name must start with \"" UNIT_NAME_PREFIX "\" and match /^[a-zA-Z0-9-]*$/.\n",
        argv[0]);
    return 1;
  }

  const char *unit_name = argv[1];

  if (!unit_name_valid(unit_name)) {
    fprintf(stderr, "Invalid unit name given\n");
    return 1;
  }

  execlp("systemctl", "systemctl", "kill", unit_name, (char*)NULL);
  perror("execvp systemctl");
  return 1;
}
