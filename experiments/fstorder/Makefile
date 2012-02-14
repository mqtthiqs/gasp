TARGET=gasp

.PHONY: all clean

OCAMLBUILD_OPT=-use-ocamlfind -classic-display
OCAMLBUILD=ocamlbuild ${OCAMLBUILD_OPT}

all: ${TARGET}

${TARGET}:
	@${OCAMLBUILD} ${TARGET}.cma ${TARGET}.top

clean:
	@${OCAMLBUILD} -clean

%:
	@$(OCAMLBUILD) $@
