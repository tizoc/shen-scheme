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
	./$(exe) script scripts/run-build-library-tests.shen
	"$(scmexe)" -q -b "$(psboot)" -b "$(csboot)" \
		--script scripts/check-build-library.ss

.PHONY: test-native
test-native: $(exe) $(runtime_artifacts)
	mkdir -p _build/native-tests
	./$(exe) script scripts/run-native-tests.shen
	./$(exe) eval -q -e "(shen-scheme.delete-file-if-exists \"_build/native-tests/cli-direct.so.scm\")"
	./$(exe) compile tests/native/simple.shen -o _build/native-tests/cli-direct.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-direct.so\")" -e "(if (= (native-test-map-inc (cons 4 (cons 5 []))) [5 6]) ok (error \"native CLI direct compile failed\"))" -e "(if (= (shen-scheme.file-exists? \"_build/native-tests/cli-direct.so.scm\") false) ok (error \"native CLI direct compile emitted scheme\"))"
	./$(exe) compile tests/native/simple.shen --profile debug -o _build/native-tests/cli-profile-debug.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-profile-debug.so\")" -e "(if (= (native-test-map-inc (cons 4 (cons 5 []))) [5 6]) ok (error \"native CLI debug profile compile failed\"))"
	./$(exe) compile tests/native/simple.shen -o _build/native-tests/cli-simple.so --emit-scheme _build/native-tests/cli-simple.scm
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-simple.so\")" -e "(if (= (native-test-map-inc (cons 4 (cons 5 []))) [5 6]) ok (error \"native CLI compile failed\"))"
	./$(exe) compile tests/native/redefinition-sealed.shen -o _build/native-tests/cli-sealed.so --emit-scheme _build/native-tests/cli-sealed.scm --mode sealed
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-sealed.so\")" -e "(if (= (native-sealed-main 5) 6) ok (error \"native CLI sealed compile failed\"))"
	./$(exe) compile tests/native/package-effects.shen -o _build/native-tests/cli-package-effects.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-package-effects.so\")" -e "(if (= (native-package-main 5) 12) ok (error \"native CLI package call failed\"))" -e "(if (= (native-package-state) [41 2]) ok (error \"native CLI package effect order failed\"))" -e "(if (package? native.test.pkg) ok (error \"native CLI package registration failed\"))" -e "(if (element? native-package-main (external native.test.pkg)) ok (error \"native CLI package external registration failed\"))" -e "(if (element? native.test.pkg.helper (internal native.test.pkg)) ok (error \"native CLI package internal registration failed\"))" -e "(if (not (= [] (assoc native.test.pkg.helper (value shen.*sigf*)))) ok (error \"native CLI package declaration failed\"))"
	./$(exe) compile-module tests/native/package-effects.shenmod -o _build/native-tests/cli-package-effects-module.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-package-effects-module.so\")" -e "(if (= (value *native-package-events*) [41 2]) ok (error \"native CLI package module effects failed\"))" -e "(if (package? native.test.pkg) ok (error \"native CLI package module registration failed\"))" -e "(if (= unavailable (trap-error (native-package-main 5) (/. E unavailable))) ok (error \"native CLI package module metadata policy failed\"))"
	./$(exe) compile-module _build/native-tests/module-decl.shenmod -o _build/native-tests/cli-module.so --emit-scheme _build/native-tests/cli-module.scm
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-module.so\")" -e "(if (= (native-module-main 5) 42) ok (error \"native CLI module declaration compile failed\"))"
	./$(exe) compile-module _build/native-tests/module-declared.shenmod -o _build/native-tests/cli-module-declared.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-module-declared.so\")" -e "(if (= (native-module-declared 41) 42) ok (error \"native CLI module declared call failed\"))" -e "(if (not (= [] (assoc native-module-declared (value shen.*sigf*)))) ok (error \"native CLI module explicit declare metadata failed\"))"
	./$(exe) compile-module _build/native-tests/module-runtime-only-declared.shenmod -o _build/native-tests/cli-module-runtime-only-declared.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-module-runtime-only-declared.so\")" -e "(if (= (native-module-runtime-only-declared 41) 42) ok (error \"native CLI module runtime-only declared call failed\"))" -e "(if (= [] (assoc native-module-runtime-only-declared (value shen.*sigf*))) ok (error \"native CLI module runtime-only declared metadata leaked\"))"
	./$(exe) compile-module _build/native-tests/module-macro.shenmod -o _build/native-tests/cli-module-macro.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-module-macro.so\")" -e "(load \"_build/native-tests/module-macro-user.shen\")" -e "(if (= (native-module-macro-user 21) 42) ok (error \"native CLI module macro metadata failed\"))"
	./$(exe) compile-module _build/native-tests/module-synonym.shenmod -o _build/native-tests/cli-module-synonym.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-module-synonym.so\")" -e "(tc +)" -e "(load \"_build/native-tests/module-synonym-user.shen\")" -e "(tc -)" -e "(if (= (native-module-synonym-user 41) 42) ok (error \"native CLI module synonym metadata failed\"))"
	./$(exe) compile-module _build/native-tests/module-datatype.shenmod -o _build/native-tests/cli-module-datatype.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-module-datatype.so\")" -e "(tc +)" -e "(load \"_build/native-tests/module-datatype-user.shen\")" -e "(tc -)" -e "(if (= (native-module-datatype-user 99) 1) ok (error \"native CLI module datatype metadata failed\"))"
	./$(exe) compile-module _build/native-tests/module-source-kl.shenmod -o _build/native-tests/cli-module-source-kl.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-module-source-kl.so\")" -e "(if (= (native-module-source-kl 41) 42) ok (error \"native CLI module source-kl call failed\"))" -e "(if (= defun (hd (ps native-module-source-kl))) ok (error \"native CLI module source-kl ps failed\"))" -e "(if (= native-module-source-kl (hd (tl (ps native-module-source-kl)))) ok (error \"native CLI module source-kl recorded wrong name\"))" -e "(if (element? native-module-source-kl (value shen.*userdefs*)) ok (error \"native CLI module source-kl userdefs failed\"))"
	./$(exe) compile-module _build/native-tests/module-no-source-kl.shenmod -o _build/native-tests/cli-module-no-source-kl.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-module-no-source-kl.so\")" -e "(if (= (native-module-no-source-kl 41) 42) ok (error \"native CLI module no-source-kl call failed\"))" -e "(if (= missing (trap-error (ps native-module-no-source-kl) (/. E missing))) ok (error \"native CLI module no-source-kl leaked ps\"))" -e "(if (not (element? native-module-no-source-kl (value shen.*userdefs*))) ok (error \"native CLI module no-source-kl leaked userdefs\"))"
	./$(exe) compile-module _build/native-tests/native.test.required.shenmod -o _build/native-tests/native.test.required.so
	./$(exe) compile-module _build/native-tests/native.test.requirer.shenmod --module-dir _build/native-tests -o _build/native-tests/native.test.requirer.so
	./$(exe) eval -q -e "(shen-scheme.load-module \"_build/native-tests/native.test.requirer.shenmod\" \"_build/native-tests\")" -e "(if (= (native-module-requirer 32) 42) ok (error \"native CLI module requires failed\"))"
	./$(exe) load-module _build/native-tests/native.test.requirer.shenmod --module-dir _build/native-tests
	./$(exe) build-module-app _build/native-tests/native.test.app-main.shenmod --module-dir _build/native-tests -o _build/native-tests/cli-module-app-wpo.so --wpo
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-module-app-wpo.so\")" -e "(if (= (value *native-module-app-init-events*) [10 11]) ok (error \"native CLI module app initializer order failed\"))" -e "(if (= (native-module-app-main 32) 42) ok (error \"native CLI module app failed\"))" -e "(load \"_build/native-tests/module-app-base-updated.shen\")" -e "(if (= (native-module-app-base 1) 1001) ok (error \"native CLI module app dependency redefine failed\"))" -e "(if (= (native-module-app-main 32) 42) ok (error \"native CLI module app lost direct dependency binding\"))"
	./$(exe) build-app tests/native/app-main.shen --module tests/native/app-lib.shen -o _build/native-tests/cli-app-wpo.so --wpo
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-app-wpo.so\")" -e "(if (= (value *native-app-init-events*) [1 12]) ok (error \"native CLI app initializer order failed\"))" -e "(if (= (native-app-main 31) 42) ok (error \"native CLI app build failed\"))" -e "(if (= (native-app-length [1 2 3]) 3) ok (error \"native CLI app runtime call failed\"))" -e "(if (= (native-app-absvector?) true) ok (error \"native CLI app absvector failed\"))" -e "(if (= (native-app-list-equal?) true) ok (error \"native CLI app generic equality failed\"))" -e "(if (= (native-app-sysfunc?) true) ok (error \"native CLI app static global failed\"))"
	./$(exe) build-app tests/native/app-main.shen --module tests/native/app-lib.shen -o _build/native-tests/cli-app-profile-wpo.so --profile wpo
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-tests/cli-app-profile-wpo.so\")" -e "(if (= (value *native-app-init-events*) [1 12]) ok (error \"native CLI WPO profile app initializer order failed\"))" -e "(if (= (native-app-main 31) 42) ok (error \"native CLI WPO profile app build failed\"))" -e "(if (= (native-app-length [1 2 3]) 3) ok (error \"native CLI WPO profile app runtime call failed\"))" -e "(if (= (native-app-absvector?) true) ok (error \"native CLI WPO profile app absvector failed\"))" -e "(if (= (native-app-list-equal?) true) ok (error \"native CLI WPO profile app generic equality failed\"))" -e "(if (= (native-app-sysfunc?) true) ok (error \"native CLI WPO profile app static global failed\"))"
	./$(exe) load-compiled _build/native-tests/cli-simple.so

.PHONY: test-native-examples
test-native-examples: $(exe) $(runtime_artifacts)
	mkdir -p _build/native-examples/modules
	cp examples/native/modules/native-example.core.shenmod _build/native-examples/modules/
	cp examples/native/modules/native-example.app.shenmod _build/native-examples/modules/
	./$(exe) compile examples/native/single-file.shen -o _build/native-examples/single-file.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-examples/single-file.so\")" -e "(if (= (answer 5) 26) ok (error \"native single-file example failed\"))"
	./$(exe) compile examples/native/binding.shen -o _build/native-examples/compatible.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-examples/compatible.so\")" -e "(load \"examples/native/binding-update.shen\")" -e "(if (= (call-helper 1) 101) ok (error \"native compatible example failed\"))"
	./$(exe) compile examples/native/binding.shen --mode sealed -o _build/native-examples/sealed.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-examples/sealed.so\")" -e "(load \"examples/native/binding-update.shen\")" -e "(if (= (call-helper 1) 2) ok (error \"native sealed example failed\"))"
	./$(exe) compile examples/native/package-effects.shen -o _build/native-examples/package-effects.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-examples/package-effects.so\")" -e "(if (= (effect-events) [\"inside-before-definition\" \"after-definition\"]) ok (error \"native package effects example failed\"))"
	./$(exe) compile-module _build/native-examples/modules/native-example.core.shenmod -o _build/native-examples/modules/native-example.core.so
	./$(exe) compile-module _build/native-examples/modules/native-example.app.shenmod --module-dir _build/native-examples/modules -o _build/native-examples/modules/native-example.app.so
	./$(exe) eval -q -e "(shen-scheme.load-module \"_build/native-examples/modules/native-example.app.shenmod\" \"_build/native-examples/modules\")" -e "(if (= (run-example 32) 42) ok (error \"native module graph example failed\"))" -e "(if (= (module-events) [42]) ok (error \"native module initializer example failed\"))"
	./$(exe) build-module-app _build/native-examples/modules/native-example.app.shenmod --module-dir _build/native-examples/modules -o _build/native-examples/app.so
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-examples/app.so\")" -e "(if (= (run-example 32) 42) ok (error \"native module app example failed\"))" -e "(if (= (module-events) [42]) ok (error \"native module app initializer example failed\"))"
	./$(exe) build-module-app _build/native-examples/modules/native-example.app.shenmod --module-dir _build/native-examples/modules -o _build/native-examples/app-wpo.so --wpo
	./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-examples/app-wpo.so\")" -e "(if (= (run-example 32) 42) ok (error \"native WPO module app example failed\"))"
	SHEN_SCHEME_RUNTIME=petite ./$(exe) eval -q -e "(shen-scheme.load-compiled \"_build/native-examples/single-file.so\")" -e "(if (= (answer 5) 26) ok (error \"native full-to-Petite example failed\"))"

.PHONY: bench-native
bench-native: $(exe) $(runtime_artifacts)
	mkdir -p _build/native-bench
	./$(exe) script scripts/bench-native-sealed.shen

.PHONY: bench-port
bench-port: $(exe) $(runtime_artifacts)
	mkdir -p _build/native-bench
	./$(exe) script benchmarks/run-port-comparison.shen $(PORT_BENCH_ARGS)

.PHONY: bench-port-smoke
bench-port-smoke: $(exe) $(runtime_artifacts)
	mkdir -p _build/native-bench
	./$(exe) script benchmarks/run-port-comparison.shen --quick

.PHONY: bench-realistic
bench-realistic: $(exe) $(runtime_artifacts)
	mkdir -p _build/native-bench
	./$(exe) script benchmarks/run-realistic-comparison.shen $(REALISTIC_BENCH_ARGS)

.PHONY: bench-realistic-smoke
bench-realistic-smoke: $(exe) $(runtime_artifacts)
	mkdir -p _build/native-bench
	./$(exe) script benchmarks/run-realistic-comparison.shen --quick

.PHONY: test-runtime-artifact-recovery
test-runtime-artifact-recovery: $(runtime_artifacts)
	$(RM) "$(runtime_obj)"
	$(MAKE) $(runtime_obj)
	test -f "$(runtime_obj)"
	test -f "$(runtime_main_obj)"
	test -f "$(runtime_lib_obj)"
	"$(scmexe)" -q -b "$(psboot)" -b "$(csboot)" \
		--script scripts/check-runtime-object.ss \
		"$(abspath $(build_dir)/lib/shen-scheme)"

.PHONY: test-clean-parallel-build
test-clean-parallel-build: chez_kernel
	$(MAKE) clean
	$(MAKE) -j4 all
	./$(exe) --version

.PHONY: test
test: test-shen test-compiler test-native test-native-examples

.PHONY: test-external-runtime
test-external-runtime: $(exe) $(runtime_artifacts)
	mkdir -p _build/external-runtime-tests
	cmp "$(petite_bootfile)" "$(psboot)"
	cmp "$(scheme_bootfile)" "$(csboot)"
	SHEN_SCHEME_HOME="$(abspath $(build_dir)/lib/shen-scheme)" ./$(exe) build-app tests/native/app-main.shen --module tests/native/app-lib.shen -o _build/external-runtime-tests/app-wpo.so --wpo
	SHEN_SCHEME_HOME="$(abspath $(build_dir)/lib/shen-scheme)" ./$(exe) script scripts/run-external-runtime-tests.shen full _build/external-runtime-tests/app-wpo.so _build/external-runtime-tests/full-compiled.so
	SHEN_SCHEME_HOME="$(abspath $(build_dir)/lib/shen-scheme)" SHEN_SCHEME_RUNTIME=petite ./$(exe) script scripts/run-external-runtime-tests.shen petite _build/external-runtime-tests/app-wpo.so _build/external-runtime-tests/petite-should-not-compile.so
ifneq ($(os), windows)
	mkdir -p "_build/external-runtime-tests/home:literal/shen-scheme"
	cp "$(petite_bootfile)" "$(scheme_bootfile)" "_build/external-runtime-tests/home:literal/"
	cp "$(runtime_obj)" "_build/external-runtime-tests/home:literal/shen-scheme/"
	SHEN_SCHEME_HOME="$(abspath _build/external-runtime-tests/home:literal)" ./$(exe) --version
endif

.PHONY: test-petite-runtime
test-petite-runtime: test-external-runtime

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
	mkdir -p "_dist/shen-scheme-$(git_tag)-$(os)-bin/docs"
	mkdir -p "_dist/shen-scheme-$(git_tag)-$(os)-bin/examples"
	mkdir -p "_dist/shen-scheme-$(git_tag)-$(os)-bin/lib/shen-scheme/shen-scheme"
	mkdir -p "_dist/shen-scheme-$(git_tag)-$(os)-bin/chez-legal"
	cp $(exe) "_dist/shen-scheme-$(git_tag)-$(os)-bin/bin"
	cp $(petite_bootfile) "_dist/shen-scheme-$(git_tag)-$(os)-bin/lib/shen-scheme"
	cp $(scheme_bootfile) "_dist/shen-scheme-$(git_tag)-$(os)-bin/lib/shen-scheme"
	cp $(runtime_obj) "_dist/shen-scheme-$(git_tag)-$(os)-bin/lib/shen-scheme/shen-scheme"
	cp README.md "_dist/shen-scheme-$(git_tag)-$(os)-bin/README.txt"
	cp docs/native-compilation.md "_dist/shen-scheme-$(git_tag)-$(os)-bin/docs"
	cp -R examples/native "_dist/shen-scheme-$(git_tag)-$(os)-bin/examples"
	cp LICENSE "_dist/shen-scheme-$(git_tag)-$(os)-bin/LICENSE.txt"
	cp $(cslicense) "_dist/shen-scheme-$(git_tag)-$(os)-bin/chez-legal/LICENSE.txt"
	cp $(cscopyright) "_dist/shen-scheme-$(git_tag)-$(os)-bin/chez-legal/NOTICE.txt"
	cd _dist; $(compress) "shen-scheme-$(git_tag)-$(os)-bin$(archiveext)" "shen-scheme-$(git_tag)-$(os)-bin"; rm -rf "shen-scheme-$(git_tag)-$(os)-bin"

.PHONY: clean
clean:
	rm -f $(exe) $(petite_bootfile) $(scheme_bootfile) $(runtime_obj) \
		$(runtime_main_obj) $(runtime_lib_obj) $(runtime_stamps) *.o *.obj
