.PHONY: clean
.DEFAULT_GOAL := all

CHIBI ?= chibi-scheme
COMPILED_DIR ?= shen/compiled
KLSOURCES_DIR ?= kl

COMPILED_DEPS = precompile.sld shen/init.sld shen/impl/compiler.scm shen/impl/declarations.scm shen/reader.sld shen/impl/reader.scm overwrites.kl
COMPILED_OUTPUT = $(COMPILED_DIR)/toplevel.kl.scm $(COMPILED_DIR)/core.kl.scm $(COMPILED_DIR)/sys.kl.scm $(COMPILED_DIR)/sequent.kl.scm $(COMPILED_DIR)/yacc.kl.scm $(COMPILED_DIR)/reader.kl.scm $(COMPILED_DIR)/prolog.kl.scm $(COMPILED_DIR)/track.kl.scm $(COMPILED_DIR)/load.kl.scm $(COMPILED_DIR)/writer.kl.scm $(COMPILED_DIR)/macros.kl.scm $(COMPILED_DIR)/declarations.kl.scm $(COMPILED_DIR)/types.kl.scm $(COMPILED_DIR)/t-star.kl.scm $(COMPILED_DIR)/extensions-common.kl.scm $(COMPILED_DIR)/extensions-chibi.kl.scm $(COMPILED_DIR)/extensions-gauche.kl.scm

$(COMPILED_DIR)/extensions-common.kl.scm: shen/extensions/common.kl $(COMPILED_DEPS)
	$(CHIBI) -m precompile -e '(compile)' < $< > $@

$(COMPILED_DIR)/extensions-chibi.kl.scm: shen/extensions/chibi.kl $(COMPILED_DEPS)
	$(CHIBI) -m precompile -e '(compile)' < $< > $@

$(COMPILED_DIR)/extensions-gauche.kl.scm: shen/extensions/gauche.kl $(COMPILED_DEPS)
	$(CHIBI) -m precompile -e '(compile)' < $< > $@

$(COMPILED_DIR)/%.kl.scm: $(KLSOURCES_DIR)/%.kl $(COMPILED_DEPS)
	$(CHIBI) -m precompile -e '(compile)' < $< > $@

all: $(COMPILED_OUTPUT)

clean:
	-$(RM) $(COMPILED_OUTPUT)
