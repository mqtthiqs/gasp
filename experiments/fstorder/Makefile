TARGET=gasp

.PHONY: all clean

OCAMLBUILD_OPT=-use-ocamlfind -classic-display -j 5
OCAMLBUILD=ocamlbuild ${OCAMLBUILD_OPT}

all: ${TARGET}

${TARGET}:
	@${OCAMLBUILD} ${TARGET}.otarget

clean:
	@${OCAMLBUILD} -clean

%:
	@$(OCAMLBUILD) $@
