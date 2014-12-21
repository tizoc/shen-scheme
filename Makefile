.PHONY: clean
.DEFAULT_GOAL := all

CHIBI ?= chibi-scheme
COMPILED_DIR ?= compiled
KLSOURCES_DIR ?= kl

COMPILED_DEPS = precompile.sld shen.sld shen/compiler.scm shen/declarations.scm shen/reader.sld shen/reader.scm overwrites.scm
COMPILED_OUTPUT = $(COMPILED_DIR)/toplevel.kl.scm $(COMPILED_DIR)/core.kl.scm $(COMPILED_DIR)/sys.kl.scm $(COMPILED_DIR)/sequent.kl.scm $(COMPILED_DIR)/yacc.kl.scm $(COMPILED_DIR)/reader.kl.scm $(COMPILED_DIR)/prolog.kl.scm $(COMPILED_DIR)/track.kl.scm $(COMPILED_DIR)/load.kl.scm $(COMPILED_DIR)/writer.kl.scm $(COMPILED_DIR)/macros.kl.scm $(COMPILED_DIR)/declarations.kl.scm $(COMPILED_DIR)/types.kl.scm $(COMPILED_DIR)/t-star.kl.scm

$(COMPILED_DIR)/%.kl.scm: $(KLSOURCES_DIR)/%.kl $(COMPILED_DEPS)
	$(CHIBI) -m precompile -e '(compile)' < $< > $@

all: $(COMPILED_OUTPUT)

clean:
	-$(RM) $(COMPILED_OUTPUT)
