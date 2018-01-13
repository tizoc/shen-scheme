ifeq ($(OS), Windows_NT)
	os = windows
	m ?= ta6nt
else ifeq ($(shell uname -s), Darwin)
	os = macos
	m ?= ta6osx
else
	os = linux
	m ?= ta6le
endif

ifeq ($(os), windows)
	S = \\\\
	objext = .obj
	binext = .exe
	cskernelname = mainmd
else
	S = /
	objext = .o
	binext =
	cskernelname = kernel
endif

csdir ?= _build$(S)csv9.5
cslicense = $(csdir)$(S)LICENSE
cscopyright = $(csdir)$(S)NOTICE
csbootpath = $(csdir)$(S)$(m)$(S)boot$(S)$(m)
psboot = .$(S)$(csbootpath)$(S)petite.boot
csboot = .$(S)$(csbootpath)$(S)scheme.boot
cskernelname ?= kernel
cskernel = $(csbootpath)$(S)$(cskernelname)$(objext)
csbinpath = $(csdir)$(S)$(m)$(S)bin$(S)$(m)
scmexe = $(csbinpath)$(S)scheme
klsources_dir ?= kl
compiled_dir ?= compiled
exe ?= shen-scheme$(binext)
prefix ?= /usr/local
bootfile_path ?= \"$(prefix)/lib/shen-scheme/shen.boot\"

git_tag ?= $(shell git tag -l --contains HEAD 2> /dev/null)
ifeq ("$(git_tag)","")
	git_tag = $(shell git rev-parse --short HEAD 2> /dev/null)
endif
archive_name = shen-scheme-$(git_tag)-src

CFLAGS += -m64

.DEFAULT: all
.PHONY: all
all: $(exe) shen.boot

$(csdir):
	echo "Downloading and uncompressing Chez..."
	mkdir -p _build
	cd _build && curl -L 'https://github.com/cisco/ChezScheme/releases/download/v9.5/csv9.5.tar.gz' | tar xz

$(cskernel): $(csdir)
	echo "Building Chez..."
	cd $(csdir) && ./configure --threads && make

$(exe): $(cskernel) main$(objext) lib/whereami$(objext)
ifeq ($(os), windows)
	cmd.exe /c "$(csdir)$(S)c$(S)vs.bat amd64 && link.exe /out:$(exe) /machine:X64 /incremental:no /release /nologo main$(objext) lib/whereami$(objext) $(csbootpath)$(S)csv95mt.lib /DEFAULTLIB:rpcrt4.lib /DEFAULTLIB:User32.lib /DEFAULTLIB:Advapi32.lib /DEFAULTLIB:Ole32.lib"
else
	$(CC) -o $@ $^ -liconv -lncurses
endif

%$(objext): %.c
ifeq ($(os), windows)
	cmd.exe /c "$(csdir)$(S)c$(S)vs.bat amd64 && cl.exe /c /nologo /W3 /D_CRT_SECURE_NO_WARNINGS /I$(csbootpath) /I.$(S)lib /MT /Fo$@ $<"
else
	$(CC) -c -o $@ $< -D DEFAULT_BOOTFILE_PATH=$(bootfile_path)  -I$(csbootpath) -I./lib -Wall -Wextra -pedantic $(CFLAGS)
endif

shen.boot: $(psboot) $(csboot) shen-scheme.scm src/* $(compiled_dir)/*.scm
	echo '(make-boot-file "shen.boot" (list)  "$(psboot)" "$(csboot)" "shen-scheme.scm")' | "$(scmexe)" -q -b "$(psboot)" -b "$(csboot)"

.PHONY: test-shen
test-shen: $(exe) shen.boot
	./$(exe) --script scripts/run-shen-tests.shen

.PHONY: test-compiler
test-compiler: $(exe) shen.boot
	./$(exe) --script scripts/run-compiler-tests.shen

.PHONY: test
test: test-shen test-compiler

.PHONY: install
install: $(exe) shen.boot
	mkdir -p $(DESTDIR)$(prefix)/bin
	mkdir -p $(DESTDIR)$(prefix)/lib/shen-scheme
	install -m 0755 $(exe) $(DESTDIR)$(prefix)/bin
	install -m 0644 shen.boot $(DESTDIR)$(prefix)/lib/shen-scheme

.PHONY: source-release
source-release:
	mkdir -p _dist
	git archive --format=tar --prefix="$(archive_name)/" $(git_tag) | (cd _dist && tar xf -)
	cp -R compiled/ "_dist/$(archive_name)/compiled"
	cp shen-scheme.scm "_dist/$(archive_name)/shen-scheme.scm"
	rm -f "_dist/$(archive_name)/".git*
	rm "_dist/$(archive_name)/"*/.gitignore
	cd _dist; tar cvzf "$(archive_name).tar.gz" "$(archive_name)/";	rm -rf "$(archive_name)/"
	echo "Generated tarball for tag $(git_tag) as _dist/$(archive_name).tar.gz"

.PHONY: clean
clean:
	rm -f $(exe) shen.boot *.o *.obj
