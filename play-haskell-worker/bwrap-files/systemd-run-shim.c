#define _GNU_SOURCE
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include "systemd-shim-inc.h"

static char* xstrdup(const char *str) {
  char *res = strdup(str);
  if (res == NULL) {
    fprintf(stderr, "systemd-run-shim: Could not allocate memory\n");
    exit(1);
  }
  return res;
}

static char* xasprintf(const char *str, ...) {
  char *dest;

  va_list ap;
  va_start(ap, str);
  int ret = vasprintf(&dest, str, ap);
  va_end(ap);

  if (ret < 0) {
    fprintf(stderr, "systemd-run-shim: Could not allocate memory\n");
    exit(1);
  }
  return dest;
}

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

  if (argc <= 1) {
    fprintf(stderr,
        "Usage: %s <unit name> [args...]\n"
        "The unit name must start with \"" UNIT_NAME_PREFIX "\" and match /^[a-zA-Z0-9-]*$/.\n",
        argv[0]);
    return 1;
  }

  const char *unit_name = argv[1];
  const int num_given_args = argc - 2;
  char **const given_args = argv + 2;

  if (!unit_name_valid(unit_name)) {
    fprintf(stderr, "Invalid unit name given\n");
    return 1;
  }

  // Note: the allocation size here must be increased if the number of
  // arguments to systemd-run increases.
  const size_t num_pre_args = 17;
  char ** const run_args = malloc((num_pre_args + num_given_args + 1) * sizeof(*run_args));

  run_args[0] = xstrdup("systemd-run");
  run_args[1] = xstrdup("--description=play-haskell-worker cpuquota");
  run_args[2] = xasprintf("--unit=%s", unit_name);
  run_args[3] = xasprintf("--uid=%u", parent_uid);
  run_args[4] = xasprintf("--gid=%u", parent_gid);
  run_args[5] = xstrdup("--pipe");
  run_args[6] = xstrdup("--wait");
  run_args[7] = xstrdup("--collect");
  run_args[8] = xstrdup("--same-dir");
  run_args[9] = xstrdup("--service-type=exec");
  run_args[10] = xasprintf("--setenv=PATH=%s", getenv("PATH"));
  run_args[11] = xstrdup("--quiet");
  run_args[12] = xstrdup("--property=CPUQuota=100%");
  // Limit memory to 600 MiB. Note that the compiled program gets a 500 MiB memory
  // limit via the GHC RTS, so this limit is 1. to constrain GHC itself (including
  // any TH code), and 2. as a second-layer defense.
  run_args[13] = xstrdup("--property=MemoryMax=2000M");
  run_args[14] = xstrdup("--property=TasksMax=50");
  run_args[15] = xstrdup("--property=LimitCORE=0");
  run_args[16] = xstrdup("--");
  // If more arguments are added above, please modify the run_args allocation above!
  for (int i = 0; i < num_given_args; i++) {
    run_args[num_pre_args + i] = xstrdup(given_args[i]);
  }
  run_args[num_pre_args + num_given_args] = NULL;

  execvp("systemd-run", run_args);
  perror("execvp systemd-run");
  return 1;
}
