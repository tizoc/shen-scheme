ifeq ($(OS), Windows_NT)
	os = windows
	m ?= ta6nt
else ifeq ($(shell uname -s), Darwin)
	os = osx
	m ?= ta6osx
else
	os = linux
	m ?= ta6le
endif

ifeq ($(os), windows)
	S = \\\\
	objext = .obj
	binext = .exe
	archiveext = .zip
	cskernelname = mainmd
	compress = 7z a -tzip
	uncompress = 7z x
	uncompressToFlag = -o
else
	S = /
	objext = .o
	binext =
	archiveext = .tar.gz
	cskernelname = kernel
	compress = tar cvzf
	uncompress = tar xzf
	uncompressToFlag = -C
endif

ifeq ($(os), linux)
	linkerflags = -lm -ldl -lpthread -luuid
endif

shenversion ?= 22.3
csversion ?= 9.5.2
build_dir ?= _build
chez_build_dir ?= $(build_dir)$(S)chez
csdir ?= $(chez_build_dir)$(S)csv$(csversion)
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
exe ?= $(build_dir)/bin/shen-scheme$(binext)
prefix ?= /usr/local
home_path ?= \"$(prefix)/lib/shen-scheme\"
bootfile = $(build_dir)/lib/shen-scheme/shen.boot

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
	mkdir -p $(chez_build_dir)
	cd $(chez_build_dir); curl -LO 'https://github.com/cisco/ChezScheme/releases/download/v$(csversion)/csv$(csversion).tar.gz'; tar xzf csv$(csversion).tar.gz; rm csv$(csversion).tar.gz

$(cskernel): $(csdir)
	echo "Building Chez..."
	cd $(csdir) && ./configure --threads && make

$(exe): $(cskernel) main$(objext)
	mkdir -p $(build_dir)/bin
ifeq ($(os), windows)
	cmd.exe /C '$(csdir)$(S)c$(S)vs.bat amd64 && link.exe /out:$(exe) /machine:X64 /incremental:no /release /nologo main$(objext) $(csbootpath)$(S)csv952mt.lib /DEFAULTLIB:rpcrt4.lib /DEFAULTLIB:User32.lib /DEFAULTLIB:Advapi32.lib /DEFAULTLIB:Ole32.lib'
else
	$(CC) -o $@ $^ $(linkerflags)
endif

%$(objext): %.c
ifeq ($(os), windows)
	cmd.exe /C '$(csdir)$(S)c$(S)vs.bat amd64 && cl.exe /c /nologo /W3 /D_CRT_SECURE_NO_WARNINGS /I$(csbootpath) /I.$(S)lib /MT /Fo$@ $<'
else
	$(CC) -c -o $@ $< -I$(csbootpath) -I./lib -Wall -Wextra -pedantic $(CFLAGS)
endif

$(bootfile): $(psboot) $(csboot) shen-scheme.scm src/* $(compiled_dir)/*.scm
	mkdir -p $(build_dir)/lib/shen-scheme
	echo '(make-boot-file "$(bootfile)" (list)  "$(psboot)" "$(csboot)" "shen-scheme.scm")' | "$(scmexe)" -q -b "$(psboot)" -b "$(csboot)"

.PHONY: fetch-kernel
fetch-kernel:
	curl -LO 'https://github.com/Shen-Language/shen-sources/releases/download/shen-$(shenversion)/ShenOSKernel-$(shenversion).tar.gz'
	tar xzf ShenOSKernel-$(shenversion).tar.gz
	cp ShenOSKernel-$(shenversion)/klambda/*.kl $(klsources_dir)/

.PHONY: fetch-prebuilt
fetch-prebuilt:
	mkdir -p $(build_dir)
	curl -LO 'https://github.com/tizoc/shen-scheme/releases/download/0.18/shen-scheme-0.18-$(os)-bin$(archiveext)'
	$(uncompress) shen-scheme-0.18-$(os)-bin$(archiveext) $(uncompressToFlag)$(build_dir)

.PHONY: precompile
precompile:
	$(build_dir)$(S)shen-scheme-0.18-$(os)-bin$(S)bin$(S)shen-scheme$(binext) --script scripts/do-build.shen > /dev/null

.PHONY: test-shen
test-shen: $(exe) $(bootfile)
	./$(exe) script scripts/run-shen-tests.shen

.PHONY: test-compiler
test-compiler: $(exe) $(bootfile)
	./$(exe) script scripts/run-compiler-tests.shen

.PHONY: test
test: test-shen test-compiler

.PHONY: run
run: $(exe) $(bootfile)
	./$(exe)

.PHONY: install
install: $(exe) $(bootfile)
	mkdir -p $(DESTDIR)$(prefix)/bin
	mkdir -p $(DESTDIR)$(home_path)
	install -m 0755 $(exe) $(DESTDIR)$(prefix)/bin
	install -m 0644 $(bootfile) $(DESTDIR)$(home_path)/

.PHONY: source-release
source-release:
	mkdir -p _dist
	git archive --format=tar --prefix="$(archive_name)/" $(git_tag) | (cd _dist && tar xf -)
	cp -R compiled/ "_dist/$(archive_name)/compiled"
	cp shen-scheme.scm "_dist/$(archive_name)/shen-scheme.scm"
	rm -f "_dist/$(archive_name)/".git*
	rm -f "_dist/$(archive_name)/".travis.yml
	rm "_dist/$(archive_name)/"*/.gitignore
	cd _dist; tar cvzf "$(archive_name).tar.gz" "$(archive_name)/";	rm -rf "$(archive_name)/"
	echo "Generated tarball for tag $(git_tag) as _dist/$(archive_name).tar.gz"

.PHONY: binary-release
binary-release: $(exe) $(bootfile)
	mkdir -p "_dist/shen-scheme-$(git_tag)-$(os)-bin"
	mkdir -p "_dist/shen-scheme-$(git_tag)-$(os)-bin/bin"
	mkdir -p "_dist/shen-scheme-$(git_tag)-$(os)-bin/lib/shen-scheme"
	mkdir -p "_dist/shen-scheme-$(git_tag)-$(os)-bin/chez-legal"
	cp $(exe) "_dist/shen-scheme-$(git_tag)-$(os)-bin/bin"
	cp $(bootfile) "_dist/shen-scheme-$(git_tag)-$(os)-bin/lib/shen-scheme"
	cp README.md "_dist/shen-scheme-$(git_tag)-$(os)-bin/README.txt"
	cp LICENSE "_dist/shen-scheme-$(git_tag)-$(os)-bin/LICENSE.txt"
	cp $(cslicense) "_dist/shen-scheme-$(git_tag)-$(os)-bin/chez-legal/LICENSE.txt"
	cp $(cscopyright) "_dist/shen-scheme-$(git_tag)-$(os)-bin/chez-legal/NOTICE.txt"
	cd _dist; $(compress) "shen-scheme-$(git_tag)-$(os)-bin$(archiveext)" "shen-scheme-$(git_tag)-$(os)-bin"; rm -rf "shen-scheme-$(git_tag)-$(os)-bin"

.PHONY: clean
clean:
	rm -f $(exe) $(bootfile) *.o *.obj
