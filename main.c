#include <scheme.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <whereami.h>

#ifdef _WIN32
#   include <windows.h>
#   define F_OK 0
#   define PATH_MAX _MAX_PATH
#   define access _access
#   define strlcpy strncpy
#else
#   include <unistd.h>
#   include <limits.h>
#endif

#ifndef DEFAULT_BOOTFILE_PATH
#   define DEFAULT_BOOTFILE_PATH NULL
#endif

static void custom_init(void) {}

int main(int argc, char *argv[]) {
  int status;
  char *bfpath = getenv("SHEN_BOOTFILE_PATH");
  char buf[PATH_MAX];

  if (bfpath == NULL) {
    if (DEFAULT_BOOTFILE_PATH != NULL) {
      bfpath = DEFAULT_BOOTFILE_PATH;
    } else {
      int dirlen;
      wai_getExecutablePath(buf, sizeof(buf), &dirlen);
      dirlen++;
      strlcpy(buf + dirlen, "shen.boot", sizeof(buf) - dirlen);
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
