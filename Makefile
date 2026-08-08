ifeq ($(OS), Windows_NT)
	os = windows
	m ?= ta6nt
else ifeq ($(shell uname -s), Darwin)
	os = macOS
	uname_m := $(shell uname -m)
	ifeq ($(uname_m), arm64)
		m ?= tarm64osx
	else
		m ?= ta6osx
	endif
else
	os = linux
	uname_m := $(shell uname -m)
	ifeq ($(uname_m), aarch64)
		m ?= tarm64le
	else ifeq ($(uname_m), riscv64)
		m ?= trv64le
	else
		m ?= ta6le
	endif
endif

ifeq ($(os), windows)
	S = \\\\
	objext = .obj
	arext = .lib
	binext = .exe
	archiveext = .zip
	cskernelname = csv1030mt
	lz4dirname = lz4mts$(S)lib
	lz4libname = liblz4
	zlibdirname = zlibmts
	zliblibname = zlib
	compress = 7z a -tzip
	uncompress = 7z x
	uncompressToFlag = -o
else
	S = /
	objext = .o
	arext = .a
	binext =
	archiveext = .tar.gz
	cskernelname = libkernel
	lz4dirname = lz4$(S)lib
	lz4libname = liblz4
	zlibdirname = zlib
	zliblibname = libz
	compress = tar cvzf
	uncompress = tar xzf
	uncompressToFlag = -C
endif

ifeq ($(os), linux)
	linkerflags = -lm -ldl -lpthread -luuid
endif

shenversion ?= 41.2
csversion ?= 10.3.0
build_dir ?= _build
chez_build_dir ?= $(build_dir)$(S)chez
csdir ?= $(chez_build_dir)$(S)csv$(csversion)
cslicense = $(csdir)$(S)LICENSE
cscopyright = $(csdir)$(S)NOTICE
csbootpath = $(csdir)$(S)$(m)$(S)boot$(S)$(m)
psboot = .$(S)$(csbootpath)$(S)petite.boot
csboot = .$(S)$(csbootpath)$(S)scheme.boot
cskernelname ?= libkernel
cskernel = $(csbootpath)$(S)$(cskernelname)$(arext)
zlibdir = $(csdir)$(S)$(m)$(S)$(zlibdirname)
zlib = $(zlibdir)$(S)$(zliblibname)$(arext)
lz4dir = $(csdir)$(S)$(m)$(S)$(lz4dirname)
lz4 = $(lz4dir)$(S)$(lz4libname)$(arext)
csbinpath = $(csdir)$(S)$(m)$(S)bin$(S)$(m)
scmexe = $(csbinpath)$(S)scheme
klsources_dir ?= kl
compiled_dir ?= compiled
exe ?= $(build_dir)/bin/shen-scheme$(binext)
prefix ?= /usr/local
home_path ?= "$(prefix)/lib/shen-scheme"
SHEN_SCHEME_OPTIMIZE_LEVEL ?= 2
SHEN_SCHEME_DEBUG_LEVEL ?= 0
SHEN_SCHEME_INSPECTOR ?= false
SHEN_SCHEME_SOURCE_INFO ?= false

petite_bootfile = $(build_dir)/lib/shen-scheme/petite.boot
scheme_bootfile = $(build_dir)/lib/shen-scheme/scheme.boot
runtime_src = shen-scheme-runtime.ss
runtime_obj = $(build_dir)/lib/shen-scheme/shen-scheme/runtime.so
runtime_main_obj = $(build_dir)/obj/shen-scheme-main.so
runtime_lib_obj = $(build_dir)/obj/shen-scheme/runtime.so
runtime_config = o$(SHEN_SCHEME_OPTIMIZE_LEVEL)-d$(SHEN_SCHEME_DEBUG_LEVEL)-i$(SHEN_SCHEME_INSPECTOR)-s$(SHEN_SCHEME_SOURCE_INFO)
runtime_stamp_prefix = $(build_dir)/obj/shen-scheme-runtime
runtime_stamp = $(runtime_stamp_prefix)-$(runtime_config).stamp
runtime_stamps = $(runtime_stamp_prefix)*.stamp
runtime_artifacts = $(petite_bootfile) $(scheme_bootfile) $(runtime_obj)
runtime_inputs = $(psboot) $(csboot) shen-scheme.scm $(runtime_src) src/* \
	$(compiled_dir)/*.scm scripts/build-runtime.ss
runtime_artifact_dirs = mkdir -p $(build_dir)/obj/shen-scheme $(build_dir)/lib/shen-scheme/shen-scheme
runtime_artifact_build = "$(scmexe)" -q -b "$(psboot)" -b "$(csboot)" --script scripts/build-runtime.ss \
	shen-scheme.scm "$(runtime_main_obj)" "$(runtime_src)" "$(runtime_lib_obj)" "$(runtime_obj)" \
	"$(SHEN_SCHEME_OPTIMIZE_LEVEL)" "$(SHEN_SCHEME_DEBUG_LEVEL)" \
	"$(SHEN_SCHEME_INSPECTOR)" "$(SHEN_SCHEME_SOURCE_INFO)"

git_tag ?= $(shell git tag -l --contains HEAD 2> /dev/null)
ifeq ("$(git_tag)","")
	git_tag = $(shell git rev-parse --short HEAD 2> /dev/null)
endif
archive_name = shen-scheme-$(git_tag)-src

ifneq ($(uname_m), aarch64)
ifneq ($(uname_m), riscv64)
	CFLAGS += -m64
endif
endif

.DEFAULT: all
.PHONY: all
all: $(exe) $(runtime_artifacts)

$(csdir):
	echo "Downloading and uncompressing Chez..."
	mkdir -p $(chez_build_dir)
	cd $(chez_build_dir); curl -LO 'https://github.com/cisco/ChezScheme/releases/download/v$(csversion)/csv$(csversion).tar.gz'; tar xzf csv$(csversion).tar.gz; rm csv$(csversion).tar.gz

$(cskernel): $(csdir)
	echo "Building Chez..."
ifeq ($(os), windows)
	cmd.exe /C 'cd $(csdir) && build.bat ta6nt'
else
	cd $(csdir) && ./configure --threads --disable-curses --disable-iconv --disable-x11 && make
endif

# The Chez build creates its kernel and stock boot files together. Declaring
# that relationship keeps fresh parallel builds from racing the boot copies.
$(psboot) $(csboot): | $(cskernel)
	test -f "$@"

.PHONY: chez_kernel
chez_kernel: $(cskernel)

$(zlib): $(cskernel)

$(lz4): $(cskernel)

$(exe): $(zlib) $(lz4) $(cskernel) main$(objext)
	mkdir -p $(build_dir)/bin
ifeq ($(os), windows)
	cmd.exe /C '$(csdir)$(S)c$(S)vs.bat amd64 && link.exe /out:$(exe) /machine:X64 /incremental:no /release /nologo $(zlib) $(lz4) $(cskernel) main$(objext) /DEFAULTLIB:rpcrt4.lib /DEFAULTLIB:User32.lib /DEFAULTLIB:Advapi32.lib /DEFAULTLIB:Ole32.lib'
else
	$(CC) -o $@ main.o -L$(csbootpath) -lkernel -L$(zlibdir) -L$(lz4dir) -llz4 -lz $(linkerflags)
endif

%$(objext): %.c
ifeq ($(os), windows)
	cmd.exe /C '$(csdir)$(S)c$(S)vs.bat amd64 && cl.exe /c /nologo /W3 /D_CRT_SECURE_NO_WARNINGS /I.$(S)$(csbootpath) /I.$(S)lib /MT /Fo$@ $<'
else
	$(CC) -c -o $@ $< -I$(csbootpath) -I./lib -Wall -Wextra -pedantic $(CFLAGS)
endif

$(petite_bootfile): $(psboot)
	mkdir -p $(build_dir)/lib/shen-scheme
	cp "$<" "$@"

$(scheme_bootfile): $(csboot)
	mkdir -p $(build_dir)/lib/shen-scheme
	cp "$<" "$@"

# Keeping only the active configuration stamp makes changing any build option,
# including changing it back, rebuild the runtime.
$(runtime_stamp): $(runtime_inputs)
	$(runtime_artifact_dirs)
	$(RM) $(runtime_stamps)
	$(runtime_artifact_build)
	touch "$@"

$(runtime_obj): | $(runtime_stamp)
	@test -f "$@" || $(runtime_artifact_dirs)
	@test -f "$@" || $(runtime_artifact_build)

.PHONY: fetch-kernel
fetch-kernel:
	curl -LO 'https://github.com/Shen-Language/shen-sources/releases/download/shen-$(shenversion)/ShenOSKernel-$(shenversion).tar.gz'
	tar xzf ShenOSKernel-$(shenversion).tar.gz
	cp ShenOSKernel-$(shenversion)/klambda/*.kl $(klsources_dir)/

.PHONY: fetch-prebuilt
fetch-prebuilt:
	mkdir -p $(build_dir)
	curl -LO 'https://github.com/tizoc/shen-scheme/releases/download/v0.26/shen-scheme-v0.26-$(os)-bin$(archiveext)'
	$(uncompress) shen-scheme-v0.26-$(os)-bin$(archiveext) $(uncompressToFlag)$(build_dir)

.PHONY: precompile-with-prebuilt
precompile-with-prebuilt:
	$(build_dir)$(S)shen-scheme-v0.26-$(os)-bin$(S)bin$(S)shen-scheme$(binext) script scripts/do-build.shen > /dev/null

.PHONY: precompile
precompile:
	$(SHEN) script scripts/do-build.shen > /dev/null

.PHONY: test-shen
test-shen: $(exe) $(runtime_artifacts)
	./$(exe) script scripts/run-shen-tests.shen

.PHONY: test-compiler
test-compiler: $(exe) $(runtime_artifacts)
	./$(exe) script scripts/run-compiler-tests.shen

.PHONY: test
test: test-shen test-compiler

.PHONY: run
run: $(exe) $(runtime_artifacts)
	./$(exe)

.PHONY: install
install: $(exe) $(runtime_artifacts)
	mkdir -p $(DESTDIR)$(prefix)/bin
	mkdir -p $(DESTDIR)$(home_path)/shen-scheme
	install -m 0755 $(exe) $(DESTDIR)$(prefix)/bin
	install -m 0644 $(petite_bootfile) $(DESTDIR)$(home_path)/
	install -m 0644 $(scheme_bootfile) $(DESTDIR)$(home_path)/
	install -m 0644 $(runtime_obj) $(DESTDIR)$(home_path)/shen-scheme/

.PHONY: source-release
source-release:
	mkdir -p _dist
	git archive --format=tar --prefix="$(archive_name)/" $(git_tag) | (cd _dist && tar xf -)
	cp $(compiled_dir)/*.scm "_dist/$(archive_name)/compiled/"
	cp shen-scheme.scm "_dist/$(archive_name)/shen-scheme.scm"
	cp $(runtime_src) "_dist/$(archive_name)/$(runtime_src)"
	rm -rf "_dist/$(archive_name)/".git*
	rm "_dist/$(archive_name)/"*/.gitignore
	cd _dist; tar cvzf "$(archive_name).tar.gz" "$(archive_name)/";	rm -rf "$(archive_name)/"
	echo "Generated tarball for tag $(git_tag) as _dist/$(archive_name).tar.gz"

.PHONY: binary-release
binary-release: $(exe) $(runtime_artifacts)
	mkdir -p "_dist/shen-scheme-$(git_tag)-$(os)-bin"
	mkdir -p "_dist/shen-scheme-$(git_tag)-$(os)-bin/bin"
	mkdir -p "_dist/shen-scheme-$(git_tag)-$(os)-bin/lib/shen-scheme/shen-scheme"
	mkdir -p "_dist/shen-scheme-$(git_tag)-$(os)-bin/chez-legal"
	cp $(exe) "_dist/shen-scheme-$(git_tag)-$(os)-bin/bin"
	cp $(petite_bootfile) "_dist/shen-scheme-$(git_tag)-$(os)-bin/lib/shen-scheme"
	cp $(scheme_bootfile) "_dist/shen-scheme-$(git_tag)-$(os)-bin/lib/shen-scheme"
	cp $(runtime_obj) "_dist/shen-scheme-$(git_tag)-$(os)-bin/lib/shen-scheme/shen-scheme"
	cp README.md "_dist/shen-scheme-$(git_tag)-$(os)-bin/README.txt"
	cp LICENSE "_dist/shen-scheme-$(git_tag)-$(os)-bin/LICENSE.txt"
	cp $(cslicense) "_dist/shen-scheme-$(git_tag)-$(os)-bin/chez-legal/LICENSE.txt"
	cp $(cscopyright) "_dist/shen-scheme-$(git_tag)-$(os)-bin/chez-legal/NOTICE.txt"
	cd _dist; $(compress) "shen-scheme-$(git_tag)-$(os)-bin$(archiveext)" "shen-scheme-$(git_tag)-$(os)-bin"; rm -rf "shen-scheme-$(git_tag)-$(os)-bin"

.PHONY: clean
clean:
	rm -f $(exe) $(petite_bootfile) $(scheme_bootfile) $(runtime_obj) \
		$(runtime_main_obj) $(runtime_lib_obj) $(runtime_stamps) *.o *.obj
