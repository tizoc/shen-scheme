// Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.
// BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

#include <scheme.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <io.h>
#include <windows.h>
#define PATH_CAPACITY (_MAX_PATH * 4)
#define PATH_SEPARATOR "\\"
#else
#include <limits.h>
#include <unistd.h>
#define PATH_CAPACITY PATH_MAX
#define PATH_SEPARATOR "/"
#endif

#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif

#ifndef R_OK
#define R_OK 4
#endif

#if defined(_WIN32) && !defined(__MINGW32__)
#define GETENV Sgetenv
#define FREE_ENV free
#else
#define GETENV getenv
#define FREE_ENV(value) ((void)(value))
#endif

static char shen_scheme_home_path[PATH_CAPACITY];
static char petite_boot_path[PATH_CAPACITY];
static char scheme_boot_path[PATH_CAPACITY];
static char runtime_object_path[PATH_CAPACITY];
static int petite_runtime;

static void fail(const char *message, const char *path) {
  if (path)
    fprintf(stderr, "ERROR: %s: %s\n", message, path);
  else
    fprintf(stderr, "ERROR: %s\n", message);
  exit(1);
}

static void copy_path(char *destination, const char *source) {
  size_t length = strlen(source);

  if (length >= PATH_CAPACITY)
    fail("path is too long", source);
  memcpy(destination, source, length + 1);
}

static int is_path_separator(char character) {
#ifdef _WIN32
  return character == '/' || character == '\\';
#else
  return character == '/';
#endif
}

static void parent_path(char *path) {
  size_t length = strlen(path);

  while (length > 1 && is_path_separator(path[length - 1]))
    path[--length] = '\0';
  while (length > 0 && !is_path_separator(path[length - 1]))
    length--;
  if (length == 0)
    fail("cannot determine the executable's parent directory", path);
  if (length == 1)
    path[1] = '\0';
  else
    path[length - 1] = '\0';
}

static void join_path(char *destination, const char *directory,
                      const char *name) {
  size_t length = strlen(directory);
  const char *separator =
      length > 0 && is_path_separator(directory[length - 1])
          ? ""
          : PATH_SEPARATOR;
  int written = snprintf(destination, PATH_CAPACITY, "%s%s%s", directory,
                         separator, name);

  if (written < 0 || written >= PATH_CAPACITY)
    fail("path is too long", directory);
}

static void executable_path(char *path) {
#ifdef _WIN32
  wchar_t wide_path[_MAX_PATH];
  char *utf8_path;
  DWORD length = GetModuleFileNameW(NULL, wide_path, _MAX_PATH);

  if (length == 0 || length >= _MAX_PATH)
    fail("cannot determine the executable path", NULL);
  wide_path[length] = L'\0';
  utf8_path = Swide_to_utf8(wide_path);
  if (utf8_path == NULL)
    fail("cannot convert the executable path to UTF-8", NULL);
  copy_path(path, utf8_path);
  free(utf8_path);
#elif defined(__APPLE__)
  uint32_t size = PATH_CAPACITY;
  char unresolved_path[PATH_CAPACITY];

  if (_NSGetExecutablePath(unresolved_path, &size) != 0)
    fail("executable path is too long", NULL);
  if (realpath(unresolved_path, path) == NULL)
    fail("cannot resolve the executable path", unresolved_path);
#else
  ssize_t length = readlink("/proc/self/exe", path, PATH_CAPACITY - 1);

  if (length < 0)
    fail("cannot determine the executable path", "/proc/self/exe");
  if (length >= PATH_CAPACITY - 1)
    fail("executable path is too long", "/proc/self/exe");
  path[length] = '\0';
#endif
}

static void require_readable(const char *message, const char *path) {
#ifdef _WIN32
  wchar_t *wide_path = Sutf8_to_wide(path);
  int readable =
      wide_path != NULL && _waccess(wide_path, R_OK) == 0;

  free(wide_path);
#else
  int readable = access(path, R_OK) == 0;
#endif

  if (!readable)
    fail(message, path);
}

static void initialize_paths(void) {
  char *home = GETENV("SHEN_SCHEME_HOME");
  char *runtime = GETENV("SHEN_SCHEME_RUNTIME");
  char executable[PATH_CAPACITY];
  char lib_directory[PATH_CAPACITY];
  char runtime_directory[PATH_CAPACITY];

  if (runtime == NULL || strcmp(runtime, "full") == 0) {
    petite_runtime = 0;
  } else if (strcmp(runtime, "petite") == 0) {
    petite_runtime = 1;
  } else {
    fail("SHEN_SCHEME_RUNTIME must be either \"full\" or \"petite\"", runtime);
  }

  if (home != NULL) {
    if (home[0] == '\0')
      fail("SHEN_SCHEME_HOME must not be empty", NULL);
    copy_path(shen_scheme_home_path, home);
  } else {
    executable_path(executable);
    parent_path(executable);
    parent_path(executable);
    join_path(lib_directory, executable, "lib");
    join_path(shen_scheme_home_path, lib_directory, "shen-scheme");
  }

  join_path(petite_boot_path, shen_scheme_home_path, "petite.boot");
  join_path(scheme_boot_path, shen_scheme_home_path, "scheme.boot");
  join_path(runtime_directory, shen_scheme_home_path, "shen-scheme");
  join_path(runtime_object_path, runtime_directory, "runtime.so");

  FREE_ENV(home);
  FREE_ENV(runtime);
}

static void validate_runtime_files(void) {
  require_readable("cannot read petite.boot", petite_boot_path);
  if (!petite_runtime)
    require_readable("cannot read scheme.boot", scheme_boot_path);
  require_readable("cannot read Shen/Scheme runtime object",
                   runtime_object_path);
}

static const char *get_shen_scheme_home_path(void) {
  return shen_scheme_home_path;
}

static ptr buf_to_bytevector(const void *buf, size_t len) {
  ptr bv = Smake_bytevector(len, 0);

  memcpy(Sbytevector_data(bv), buf, len);

  return bv;
}

static void load_program(const char *path) {
  ptr program = Sstring_utf8(path, -1);

  Slock_object(program);
  Scall1(Stop_level_value(Sstring_to_symbol("load-program")), program);
  Sunlock_object(program);
}

static int shen_scheme_main(int argc, char *argv[]) {
  int index, status;
  const char **scheme_argv;

  initialize_paths();
  validate_runtime_files();

  Sscheme_init(NULL);
  Sregister_boot_file(petite_boot_path);
  if (!petite_runtime)
    Sregister_boot_file(scheme_boot_path);
  Sbuild_heap(NULL, NULL);

  Sforeign_symbol("get_shen_scheme_home_path", (void*)get_shen_scheme_home_path);
  Sforeign_symbol("scm_make_utf8_string", (void*)Sstring_utf8);
  Sforeign_symbol("scm_make_bytevector", (void*)buf_to_bytevector);
  load_program(runtime_object_path);

  scheme_argv = malloc((size_t)(argc + 2) * sizeof(*scheme_argv));
  if (scheme_argv == NULL)
    fail("cannot allocate the Scheme argument vector", NULL);
  scheme_argv[0] = argv[0];
  for (index = 0; index < argc; index++)
    scheme_argv[index + 1] = argv[index];
  scheme_argv[argc + 1] = NULL;

  status = Sscheme_start(argc + 1, scheme_argv);
  free(scheme_argv);
  Sscheme_deinit();

  return status;
}

#if defined(_WIN32) && !defined(__MINGW32__)
int wmain(int argc, wchar_t *wide_argv[], wchar_t *wide_envp[]) {
  char **argv = malloc((size_t)(argc + 1) * sizeof(*argv));
  int index, status;

  (void)wide_envp;
  if (argv == NULL)
    fail("cannot allocate the UTF-8 argument vector", NULL);
  for (index = 0; index < argc; index++) {
    argv[index] = Swide_to_utf8(wide_argv[index]);
    if (argv[index] == NULL)
      fail("cannot convert a command-line argument to UTF-8", NULL);
  }
  argv[argc] = NULL;

  status = shen_scheme_main(argc, argv);
  for (index = 0; index < argc; index++)
    free(argv[index]);
  free(argv);

  return status;
}
#else
int main(int argc, char *argv[]) {
  return shen_scheme_main(argc, argv);
}
#endif
