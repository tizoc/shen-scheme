.PHONY: clean test test-shen test-compiler download-chez

m ?= $(shell sh scripts/chez-target.sh)
csdir = _build/csv9.5
bootpath = $(csdir)/$m/boot/$m
psboot = $(bootpath)/petite.boot
csboot = $(bootpath)/scheme.boot
kernel = $(bootpath)/kernel.o
binpath = $(csdir)/$m/bin
scmexe = $(binpath)/scheme

CFLAGS += -m64

all: shen-scheme shen.boot

$(csdir):
	echo "Downloading and uncompressing Chez..."
	mkdir -p _build
	cd _build && curl -L 'https://github.com/cisco/ChezScheme/releases/download/v9.5/csv9.5.tar.gz' | tar xz

$(kernel): $(csdir)
	echo "Building Chez..."
	cd $(csdir) && ./configure --threads && make

shen-scheme: $(kernel) main.o
	$(CC) -o $@ main.o $(kernel) -liconv -lncurses

main.o: main.c
	$(CC) -c -o $@ $< -I$(bootpath) -Wall -Wextra -pedantic $(CFLAGS)

shen.boot: $(psboot) $(csboot) shen-chez.scm src/* compiled/*.scm
	echo '(make-boot-file "shen.boot" (list)  "./$(psboot)" "./$(csboot)" "shen-chez.scm")' | "$(scmexe)" -q -b "./$(psboot)" -b "./$(csboot)"

test-shen: shen-scheme shen.boot
	./shen-scheme --script scripts/run-shen-tests.shen

test-compiler: shen-scheme shen.boot
	./shen-scheme --script scripts/run-compiler-tests.shen

test: test-shen test-compiler

clean:
	rm -f shen-scheme shen.boot *.o
