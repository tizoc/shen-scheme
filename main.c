#include "scheme.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#   include <windows.h>
#   define F_OK 0
#   define PATH_MAX _MAX_PATH
#   define PATH_SEPARATOR '\\'
#   define access _access
#   define strlcpy strncpy
#   define realpath(path, expanded) GetFullPathName(path, PATH_MAX, expanded, NULL)
#else
#   include <unistd.h>
#   include <limits.h>
#   define PATH_SEPARATOR '/'
#endif

#ifndef DEFAULT_BOOTFILE_PATH
#   define DEFAULT_BOOTFILE_PATH NULL
#endif

static void custom_init(void) {}

int main(int argc, char *argv[]) {
  int status;
  char *bfpath = getenv("SHEN_BOOTFILE_PATH");

  if (bfpath == NULL) {
    if (DEFAULT_BOOTFILE_PATH != NULL) {
      bfpath = DEFAULT_BOOTFILE_PATH;
    } else {
      char buf[PATH_MAX];
      char *last_slash;

      realpath(argv[0], buf);
      last_slash = strrchr(buf, PATH_SEPARATOR) + 1;
      strlcpy(last_slash, "shen.boot", last_slash - buf);
      bfpath = buf;
    }
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
