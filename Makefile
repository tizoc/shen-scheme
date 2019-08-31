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

csversion ?= 9.5.2
csdir ?= _build$(S)csv$(csversion)
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
home_path ?= \"$(prefix)/lib/shen-scheme\"
bootfile = boot/shen.boot

git_tag ?= $(shell git tag -l --contains HEAD 2> /dev/null)
ifeq ("$(git_tag)","")
	git_tag = $(shell git rev-parse --short HEAD 2> /dev/null)
endif
archive_name = shen-scheme-$(git_tag)-src

CFLAGS += -m64

.DEFAULT: all
.PHONY: all
all: $(exe) $(bootfile)

$(csdir):
	echo "Downloading and uncompressing Chez..."
	mkdir -p _build
	cd _build && curl -L 'https://github.com/cisco/ChezScheme/releases/download/v$(csversion)/csv$(csversion).tar.gz' | tar xz

$(cskernel): $(csdir)
	echo "Building Chez..."
	cd $(csdir) && ./configure --threads && make

$(exe): $(cskernel) main$(objext)
ifeq ($(os), windows)
	cmd.exe /c "$(csdir)$(S)c$(S)vs.bat amd64 && link.exe /out:$(exe) /machine:X64 /incremental:no /release /nologo main$(objext) $(csbootpath)$(S)csv95mt.lib /DEFAULTLIB:rpcrt4.lib /DEFAULTLIB:User32.lib /DEFAULTLIB:Advapi32.lib /DEFAULTLIB:Ole32.lib"
else
	$(CC) -o $@ $^ -lm -ldl -lpthread
endif

%$(objext): %.c
ifeq ($(os), windows)
	cmd.exe /c "$(csdir)$(S)c$(S)vs.bat amd64 && cl.exe /c /nologo /W3 /D_CRT_SECURE_NO_WARNINGS /I$(csbootpath) /I.$(S)lib /MT /Fo$@ $<"
else
	$(CC) -c -o $@ $< -D DEFAULT_SHEN_SCHEME_HOME_PATH=$(home_path)  -I$(csbootpath) -I./lib -Wall -Wextra -pedantic $(CFLAGS)
endif

$(bootfile): $(psboot) $(csboot) shen-scheme.scm src/* $(compiled_dir)/*.scm
	echo '(make-boot-file "$(bootfile)" (list)  "$(psboot)" "$(csboot)" "shen-scheme.scm")' | "$(scmexe)" -q -b "$(psboot)" -b "$(csboot)"

.PHONY: test-shen
test-shen: $(exe) $(bootfile)
	env SHEN_SCHEME_HOME=. ./$(exe) --script scripts/run-shen-tests.shen

.PHONY: test-compiler
test-compiler: $(exe) $(bootfile)
	env SHEN_SCHEME_HOME=. ./$(exe) --script scripts/run-compiler-tests.shen

.PHONY: test
test: test-shen test-compiler

.PHONY: run
run: $(exe) $(bootfile)
	env SHEN_SCHEME_HOME=. ./$(exe)

.PHONY: install
install: $(exe) $(bootfile)
	mkdir -p $(DESTDIR)$(prefix)/bin
	mkdir -p $(DESTDIR)$(home_path)
	install -m 0755 $(exe) $(DESTDIR)$(prefix)/bin
	install -m 0644 $(bootfile) $(DESTDIR)$(home_path)/boot

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
	rm -f $(exe) $(bootfile) *.o *.obj
