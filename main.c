// Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.
// BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

#include <scheme.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#   include <windows.h>
#   include <io.h>
#   define F_OK 0
#   define PATH_MAX _MAX_PATH
#   define access _access
#   define PATH_SEPARATOR "\\"
#else
#   include <unistd.h>
#   include <limits.h>
#   define PATH_SEPARATOR "/"
#   ifdef __APPLE__
#     include <mach-o/dyld.h>
#     define strcpy_s(dest, size, src) strlcpy(dest, src, size)
#   else
#     define strcpy_s(dest, size, src) snprintf(dest, size, "%s", src)
#   endif
#endif

static char shen_scheme_home_path[PATH_MAX];
static char shen_scheme_bootfile_path[PATH_MAX];

static void initialize_paths(void) {
  char *sshpath = getenv("SHEN_SCHEME_HOME");
  char *ssbfpath = getenv("SHEN_SCHEME_BOOT");
#ifdef __APPLE__
  char tmpbuf[PATH_MAX];
#endif

  if (sshpath) {
    strcpy_s(shen_scheme_home_path, PATH_MAX, sshpath);
  } else {
#if defined(_WIN32)
    HMODULE hModule = GetModuleHandle(NULL);
    GetModuleFileName(hModule, shen_scheme_home_path, PATH_MAX);
#elif defined(__APPLE__)
    uint32_t bufsize = PATH_MAX;
    _NSGetExecutablePath(tmpbuf, &bufsize);
    realpath(tmpbuf, shen_scheme_home_path);
#else
    readlink("/proc/self/exe", shen_scheme_home_path, PATH_MAX);
#endif
    for (int slashes = 2; slashes > 0; --slashes) {
      for (size_t i = strlen(shen_scheme_home_path); i >=0; --i) {
        if (shen_scheme_home_path[i] == PATH_SEPARATOR[0]) {
          shen_scheme_home_path[i] = '\0';
          break;
        }
      }
    }
    strncat(shen_scheme_home_path, "/lib/shen-scheme",
            PATH_MAX - strlen(shen_scheme_home_path) - 1);
  }

  if (ssbfpath) {
    strcpy_s(shen_scheme_bootfile_path, PATH_MAX, ssbfpath);
  } else {
    snprintf(shen_scheme_bootfile_path, PATH_MAX, "%s%sshen.boot",
             shen_scheme_home_path,PATH_SEPARATOR);
  }
}

static const char *get_shen_scheme_home_path(void) {
  return shen_scheme_home_path;
}

static ptr buf_to_bytevector(const void *buf, size_t len) {
  ptr bv = Smake_bytevector(len, 0);

  memcpy(Sbytevector_data(bv), buf, len);

  return bv;
}

int main(int argc, char *argv[]) {
  int status;

  initialize_paths();

  if (access(shen_scheme_bootfile_path, F_OK) == -1) {
    fprintf(stderr, "ERROR: boot file '%s' doesn't exist or is not readable.\n",
            shen_scheme_bootfile_path);
    exit(1);
  }

  Sscheme_init(NULL);
  Sregister_boot_file(shen_scheme_bootfile_path);
  Sbuild_heap(NULL, NULL);
  Sforeign_symbol("get_shen_scheme_home_path", (void*)get_shen_scheme_home_path);
  Sforeign_symbol("scm_make_utf8_string", (void*)Sstring_utf8);
  Sforeign_symbol("scm_make_bytevector", (void*)buf_to_bytevector);
  status = Sscheme_start(argc + 1, (const char**)argv - 1);
  Sscheme_deinit();

  exit(status);
}
