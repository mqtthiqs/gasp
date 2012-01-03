TARGET=gasp

.PHONY: all clean

OCAMLBUILD_OPT=-use-ocamlfind -classic-display
OCAMLBUILD=ocamlbuild ${OCAMLBUILD_OPT}

all: ${TARGET}

${TARGET}:
	@${OCAMLBUILD} ${TARGET}.byte

clean:
	@${OCAMLBUILD} -clean

%:
	@$(OCAMLBUILD) $@
