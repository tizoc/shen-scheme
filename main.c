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
#endif

#ifdef __APPLE__
#   define strcpy_s(dest, max, src) strlcpy(dest, src, max)
#endif

#ifndef DEFAULT_SHEN_SCHEME_HOME_PATH
#   define DEFAULT_SHEN_SCHEME_HOME_PATH NULL
#endif

#ifndef DEFAULT_SHEN_SCHEME_BOOTFILE_PATH
#   define DEFAULT_SHEN_SCHEME_BOOTFILE_PATH NULL
#endif

static char shen_scheme_home_path[PATH_MAX];
static char shen_scheme_bootfile_path[PATH_MAX];

static void initialize_paths() {
  char *sshpath = getenv("SHEN_SCHEME_HOME");
  char *ssbfpath = getenv("SHEN_SCHEME_BOOT");

  if (sshpath) {
    strcpy_s(shen_scheme_home_path, PATH_MAX, sshpath);
  } else {
    if (DEFAULT_SHEN_SCHEME_HOME_PATH != NULL) {
      strcpy_s(shen_scheme_home_path, PATH_MAX, DEFAULT_SHEN_SCHEME_HOME_PATH);
    } else {
#ifdef _WIN32
      /* On Windows, use the executable directory as home path */
      HMODULE hModule = GetModuleHandle(NULL);
      GetModuleFileName(hModule, shen_scheme_home_path, PATH_MAX);
      for (size_t i = strlen(shen_scheme_home_path); i >=0; --i) {
        if (shen_scheme_home_path[i] == '\\')
        {
          shen_scheme_home_path[i] = '\0';
          break;
        }
      }
#else
      fputs("ERROR: no SHEN_SCHEME_HOME path was specified", stderr);
      exit(1);
#endif
    }
  }

  if (ssbfpath) {
    strcpy_s(shen_scheme_bootfile_path, PATH_MAX, ssbfpath);
  } else {
    snprintf(shen_scheme_bootfile_path, PATH_MAX, "%s%sboot%sshen.boot",
             shen_scheme_home_path, PATH_SEPARATOR, PATH_SEPARATOR);
  }
}

static const char *get_shen_scheme_home_path() {
  return shen_scheme_home_path;
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
  status = Sscheme_start(argc + 1, (const char**)argv - 1);
  Sscheme_deinit();

  exit(status);
}

// These are to avoid the need for linking in ncurses

void *setupterm;
void *tputs;
void *cur_term;

// These are to avoid the need for linking in iconv

void *iconv;
void *iconv_close;
void *iconv_open;
