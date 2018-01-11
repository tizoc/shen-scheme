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
	cmd.exe /c "$(csdir)$(S)c$(S)vs.bat amd64 && link.exe /out:$(exe) /machine:X64 /incremental:no /release /nologo main$(objext) $(csbinpath)$(S)csv95.lib  /DEFAULTLIB:rpcrt4.lib /DEFAULTLIB:User32.lib /DEFAULTLIB:Advapi32.lib /DEFAULTLIB:Ole32.lib"
else
	$(CC) -o $@ $^ -liconv -lncurses
endif

%$(objext): %.c
ifeq ($(os), windows)
	cmd.exe /c "$(csdir)$(S)c$(S)vs.bat amd64 && cl.exe /c /nologo /W3 /D_CRT_SECURE_NO_WARNINGS /I$(csbootpath) /I.$(S)lib /MD /Fo$@ $<"
else
	$(CC) -c -o $@ $< -I$(csbootpath) -I./lib -Wall -Wextra -pedantic $(CFLAGS)
endif

shen.boot: $(psboot) $(csboot) shen-chez.scm src/* $(compiled_dir)/*.scm
	echo '(make-boot-file "shen.boot" (list)  "$(psboot)" "$(csboot)" "shen-chez.scm")' | "$(scmexe)" -q -b "$(psboot)" -b "$(csboot)"

.PHONY: test-shen
test-shen: $(exe)shen.boot
	./$(exe) --script scripts/run-shen-tests.shen

.PHONY: test-compiler
test-compiler: $(exe) shen.boot
	./$(exe) --script scripts/run-compiler-tests.shen

.PHONY: test
test: test-shen test-compiler

.PHONY: clean
clean:
	rm -f $(exe) shen.boot *.o *.obj
