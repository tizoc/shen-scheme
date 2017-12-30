#include "scheme.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#ifndef DEFAULT_BOOTFILE_PATH
#  define DEFAULT_BOOTFILE_PATH "./shen.boot"
#endif

static void custom_init(void) {}

int main(int argc, char *argv[]) {
  int status;
  char *bfpath = getenv("SHEN_BOOTFILE_PATH");

  if (bfpath == NULL) {
    bfpath = DEFAULT_BOOTFILE_PATH;
  }

  if (access(bfpath, F_OK) == -1) {
    fprintf(stderr, "ERROR: boot file '%s' doesn't exist or is not readable.\n",
            bfpath);
    exit(1);
  }

  Sscheme_init(NULL);
  Sregister_boot_file(bfpath);
  Sbuild_heap(NULL, custom_init);
  status = Sscheme_start(argc + 1, (const char**)argv - 1);
  Sscheme_deinit();

  exit(status);
}
